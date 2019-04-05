source("Code/library.R")

## Get vector of districts/district IDs
district_names_2003 <- read_csv("Data/Raw/electoral_districts_key_2003.csv")$name_english %>% as.character()
Encoding(district_names_2003) <- "UTF-8"
district_ids_2003 <- read_csv("Data/Raw/electoral_districts_key_2003.csv")$district_code

education_table_list_2001 <- age_table_list_2001 <- sex_table_list_2001 <- minority_table_list_2001 <- vector("list", 308)

## Run through districts
for(i in 1:308) {
  ## Data URL
  census_url <- paste0("https://www12.statcan.gc.ca/english/census01/products/standard/fedprofile/RetrieveTable.cfm?R=FED03&G=", 
                       district_ids_2003[i])
  
  ## Scrape
  census_GET <- httr::GET(census_url) %>% 
    httr::content(as = "text", encoding = "UTF-8") %>%
    xml2::read_html()
  header_string <- '//table//table'
  census_table <- (rvest::html_nodes(census_GET, xpath = header_string) %>%
                     # Parse as a table
                     rvest::html_table(fill = TRUE))[[1]][,1:2] %>%
    as.tbl() %>%
    dplyr::select(characteristic = 1, pop = 2) %>%
    mutate(pop = gsub(",", "", pop) %>% as.numeric())
  
  ## Extract education, age, sex, and visible minority tables
  education_table_list_2001[[i]] <- census_table %>%
    filter(grepl("high school|diploma", characteristic, ignore.case = TRUE))
  
  age_table_list_2001[[i]] <- census_table %>%
    filter(!grepl("[[:alpha:]]", characteristic)) %>%
    group_by(characteristic) %>%
    mutate(sex = 1:n(),
           sex = case_when(sex == 1 ~ "Male",
                           sex == 2 ~ "Female"),
           age = case_when(characteristic %in% c("0-4", "5-9", "10-14", "15") ~ "0 to 15 years",
                           characteristic %in% c("15-19", "20-24", "25-29") ~ "15 to 29 years",
                           characteristic %in% c("30-34", "35-39", "40-44") ~ "30 to 44 years",
                           characteristic %in% c("45-49", "50-54", "55-59", "60-64") ~ "45 to 64 years",
                           characteristic %in% c("65-69", "70-74", "75-79", "80-84", "85+") ~ "65 and older")) %>%
    na.omit() %>%
    group_by(age, sex) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()
  
  sex_table_list_2001[[i]] <- age_table_list_2001[[i]] %>%
    group_by(sex) %>%
    summarise(pop = sum(pop))
  
  minority_table_list_2001[[i]] <- census_table %>%
    filter(grepl("Total visible minority population", characteristic) | characteristic == "All others") %>%
    mutate(minority = case_when(grepl("Total visible minority population", characteristic) ~ "minority",
                                characteristic == "All others" ~ "white"))
}

names(education_table_list_2001) <- names(age_table_list_2001) <- names(sex_table_list_2001) <- names(minority_table_list_2001) <- district_names_2003

education_pct_list_2001 <- education_table_list_2001 %>%
  lapply(function(df) {
    pct_df <- df %>%
      mutate(pct = pop/sum(pop),
             education = case_when(grepl("Less than", characteristic) ~ "educ_hsless",
                                   grepl("some post", characteristic) ~ "educ_hsless",
                                   grepl("Trades", characteristic) ~ "educ_college",
                                   grepl("College", characteristic) ~ "educ_college",
                                   grepl("University", characteristic) ~ "educ_university")) %>%
      group_by(education) %>%
      summarise(pct = sum(pct)) %>%
      spread(education, pct)
    return(pct_df)
  })

age_pct_list_2001 <- age_table_list_2001 %>%
  lapply(function(df) {
    pct_df <- df %>%
      filter(grepl("29|44|64|65", age)) %>%
      mutate(age = case_when(age == "15 to 29 years" ~ "age_1529",
                             age == "30 to 44 years" ~ "age_3044",
                             age == "45 to 64 years" ~ "age_4564",
                             age == "65 and older" ~ "age_65"),
             pct = pop/sum(pop)) %>%
      group_by(age) %>%
      summarise(pct = sum(pct)) %>%
      spread(age, pct)
    return(pct_df)
  })

sex_pct_list_2001 <- sex_table_list_2001 %>%
  lapply(function(df) {
    pct_df <- df %>%
      mutate(pct = pop/sum(pop)) %>%
      dplyr::select(sex, pct) %>%
      mutate(sex = case_when(sex == "Female" ~ "sex_female",
                             sex == "Male" ~ "sex_male")) %>%
      spread(sex, pct)
    return(pct_df)
  })

minority_pct_list_2001 <- minority_table_list_2001 %>%
  lapply(function(df) {
    pct_df <- df %>%
      mutate(pct = pop/sum(pop)) %>%
      dplyr::select(minority, pct) %>%
      spread(minority, pct)
    return(pct_df)
  })

## Make into a tibble
educ_pct_tbl_2001 <- bind_rows(education_pct_list_2001)
age_pct_tbl_2001 <- bind_rows(age_pct_list_2001)
sex_pct_tbl_2001 <- bind_rows(sex_pct_list_2001)
minority_pct_tbl_2001 <- bind_rows(minority_pct_list_2001)

demographics_2001 <- bind_cols(age_pct_tbl_2001, educ_pct_tbl_2001, sex_pct_tbl_2001, minority_pct_tbl_2001) %>%
  mutate(district_code = district_ids_2003) %>%
  dplyr::select(district_code, everything())

write_csv(demographics_2001, "Data/Processed/2001_demographics.csv")
