source("Code/library.R")

## Get vector of districts/district IDs
district_names_2003 <- read_csv("Data/Raw/electoral_districts_key_2003.csv")$name_english %>% as.character()
Encoding(district_names_2003) <- "UTF-8"
district_ids_2003 <- read_csv("Data/Raw/electoral_districts_key_2003.csv")$district_code

education_table_list_2006 <- age_table_list_2006 <- sex_table_list_2006 <- minority_table_list_2006 <- vector("list", 308)

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
  census_table <- (rvest::html_nodes(census_GET, xpath = header_string) %>%
    # Parse as a table
    rvest::html_table(fill = TRUE))[[1]] %>%
    as.tbl() %>%
    dplyr::select(characteristic = 1, pop = 2) %>%
    mutate(pop = gsub(",", "", pop) %>% as.numeric()) %>%
    na.omit()
  
  ## Extract education, age, and sex tables
  education_table_list_2006[[i]] <- census_table %>%
    mutate(education = case_when(grepl("No certificate|High school", characteristic) ~ "educ_hsless",
                                 grepl("Apprentice|College, CEGEP", characteristic) ~ "educ_college",
                                 grepl("bachelor", characteristic) ~ "educ_university")) %>%
    na.omit() %>%
    distinct(characteristic, .keep_all = TRUE) %>%
    group_by(education) %>%
    summarise(pop = sum(pop))
  
  age_table_list_2006[[i]] <- census_table %>%
    mutate(age = case_when(characteristic %in% c("15 to 19 years", "20 to 24 years", "25 to 29 years") ~ "age_1529",
                           characteristic %in% c("30 to 34 years", "35 to 39 years", "40 to 44 years") ~ "age_3044",
                           characteristic %in% c("45 to 49 years", "50 to 54 years", "55 to 59 years", "60 to 64 years") ~ "age_4564",
                           characteristic %in% c("65 to 69 years", "70 to 74 years", "75 to 79 years", "80 to 84 years", "85 years and over") ~ "age_65")
    ) %>%
    na.omit() %>%
    distinct(characteristic, .keep_all = TRUE) %>%
    group_by(age) %>%
    summarise(pop = sum(pop))
  
  sex_table_list_2006[[i]] <- census_table %>%
    mutate(sex = case_when(characteristic == "Male, total" ~ "sex_male",
                           characteristic == "Female, total" ~ "sex_female")) %>%
    na.omit() %>%
    dplyr::select(sex, pop)
  
  minority_table_list_2006[[i]] <- census_table %>%
    filter(grepl("Total visible minority population|Not a visible minority", characteristic)) %>%
    mutate(minority = case_when(grepl("Total visible minority population", characteristic) ~ "minority",
                                grepl("Not a visible minority", characteristic) ~ "white"))
}

names(education_table_list_2006) <- names(age_table_list_2006) <- names(sex_table_list_2006) <- names(minority_table_list_2006) <- district_names_2003

education_pct_list_2006 <- education_table_list_2006 %>%
  lapply(function(df) {
    pct_df <- df %>%
      mutate(pct = pop/sum(pop)) %>%
      dplyr::select(education, pct) %>%
      spread(education, pct)
    return(pct_df)
  })

age_pct_list_2006 <- age_table_list_2006 %>%
  lapply(function(df) {
    pct_df <- df %>%
      mutate(pct = pop/sum(pop)) %>%
      dplyr::select(age, pct) %>%
      spread(age, pct)
    return(pct_df)
  })

sex_pct_list_2006 <- sex_table_list_2006 %>%
  lapply(function(df) {
    pct_df <- df %>%
      mutate(pct = pop/sum(pop)) %>%
      dplyr::select(sex, pct) %>%
      spread(sex, pct)
    return(pct_df)
  })

minority_pct_list_2006 <- minority_table_list_2006 %>%
  lapply(function(df) {
    pct_df <- df %>%
      mutate(pct = pop/sum(pop)) %>%
      dplyr::select(minority, pct) %>%
      spread(minority, pct)
    return(pct_df)
  })


## Make into a tibble
educ_pct_tbl_2006 <- bind_rows(education_pct_list_2006)
age_pct_tbl_2006 <- bind_rows(age_pct_list_2006)
sex_pct_tbl_2006 <- bind_rows(sex_pct_list_2006)
minority_pct_tbl_2006 <- bind_rows(minority_pct_list_2006)

demographics_2006 <- bind_cols(age_pct_tbl_2006, educ_pct_tbl_2006, sex_pct_tbl_2006, minority_pct_tbl_2006) %>%
  mutate(district_code = district_ids_2003) %>%
  dplyr::select(district_code, everything())

write_csv(demographics_2006, "Data/Processed/2006_demographics.csv")
