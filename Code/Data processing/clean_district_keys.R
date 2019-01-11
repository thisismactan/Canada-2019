#### Clean district keys ####
source("Code/library.R")

district_key_2003 <- read_csv("Data/Raw/electoral_districts_key_2003.csv")
Encoding(district_key_2003$name_english) <- "LATIN1"
district_key_2003 <- district_key_2003 %>%
  mutate(name_english = gsub("[[:digit:]]|[[:space:]][[:digit:]]", "", name_english))

district_key_2013 <- read_csv("Data/Raw/electoral_districts_key_2013.csv")
district_key_2013 <- district_key_2013 %>%
  mutate(name_english = gsub("[[:digit:]]|[[:space:]][[:digit:]]", "", name_english),
         name_english = gsub("--", "–", name_english))
