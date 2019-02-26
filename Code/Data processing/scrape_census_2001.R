source("Code/library.R")

## Get vector of districts/district IDs
district_names_2003 <- read_csv("Data/Raw/electoral_districts_key_2003.csv")$name_english %>% as.character()
Encoding(district_names_2003) <- "UTF-8"
district_ids_2003 <- read_csv("Data/Raw/electoral_districts_key_2003.csv")$district_code

education_table_list <- age_table_list <- sex_table_list <- vector("list", 308)

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
                     rvest::html_table(fill = TRUE))[[1]] %>%
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
