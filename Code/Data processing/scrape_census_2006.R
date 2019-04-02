source("Code/library.R")

## Get vector of districts/district IDs
district_names_2003 <- read_csv("Data/Raw/electoral_districts_key_2003.csv")$name_english %>% as.character()
Encoding(district_names_2003) <- "UTF-8"
district_ids_2003 <- read_csv("Data/Raw/electoral_districts_key_2003.csv")$district_code

education_table_list <- age_table_list <- sex_table_list <- vector("list", 308)

## Run through districts
for(i in 1:308) {
  ## Data URL
  census_url <- paste0("https://www12.statcan.gc.ca/census-recensement/2006/dp-pd/prof/92-595/P2C.cfm?TPL=RETR&LANG=E&GC=", 
                       district_ids_2003[i])
  
  ## Scrape
  census_GET <- httr::GET(census_url) %>% 
    httr::content(as = "text") %>%
    xml2::read_html()
  header_string <- '//table'
  census_table <- rvest::html_nodes(census_GET, xpath = header_string) %>%
    # Parse as a table
    rvest::html_table(fill = TRUE) %>%
    as.tbl() %>%
    dplyr::select(characteristic = 1, pop = 2) %>%
    mutate(pop = gsub(",", "", pop) %>% as.numeric())
  
  ## Extract education, age, and sex tables
  education_table_list[[i]] <- census_table %>%
    filter(grepl("high school|diploma", characteristic, ignore.case = TRUE))
  
  age_table_list[[i]] <- census_table %>%
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
  
  sex_table_list[[i]] <- age_table_list[[i]] %>%
    group_by(sex) %>%
    summarise(pop = sum(pop))
}

names(education_table_list) <- names(age_table_list) <- names(sex_table_list) <- district_names_2003

education_pct_list <- education_table_list %>%
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

age_pct_list <- age_table_list %>%
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

sex_pct_list <- sex_table_list %>%
  lapply(function(df) {
    pct_df <- df %>%
      mutate(pct = pop/sum(pop)) %>%
      dplyr::select(sex, pct) %>%
      mutate(sex = case_when(sex == "Female" ~ "sex_female",
                             sex == "Male" ~ "sex_male")) %>%
      spread(sex, pct)
    return(pct_df)
  })

## Make into a tibble
educ_pct_tbl <- bind_rows(education_pct_list)
age_pct_tbl <- bind_rows(age_pct_list)
sex_pct_tbl <- bind_rows(sex_pct_list)

demographics_2001 <- bind_cols(educ_pct_tbl, age_pct_tbl, sex_pct_tbl) %>%
  mutate(district_code = district_ids_2003) %>%
  dplyr::select(district_code, name_english, everything())
  