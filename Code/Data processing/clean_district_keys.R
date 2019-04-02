#### Clean district keys ####
source("Code/library.R")

district_key_2003 <- read_csv("Data/Raw/electoral_districts_key_2003.csv")
Encoding(district_key_2003$name_english) <- "UTF-8"
district_key_2003 <- district_key_2003 %>%
  mutate(name_english = gsub("[[:digit:]]|[[:space:]][[:digit:]]", "", name_english))

district_key_2013 <- readr::read_csv("Data/Raw/electoral_districts_key_2013.csv", locale = locale(encoding = "LATIN1"))
district_key_2013 <- district_key_2013 %>%
  mutate(name_english = gsub("[[:digit:]]|[[:space:]][[:digit:]]", "", name_english),
         name_english = gsub("--", "–", name_english),
         name_english = gsub("â€“", "–", name_english), 
         name_english = gsub("--", "–", name_english),
         name_english = gsub("Ã©", "é", name_english),
         name_english = gsub("Ã¨", "è", name_english),
         name_english = gsub("ÃŽ", "Î", name_english),
         name_english = gsub("Ã‰", "É", name_english),
         name_english = gsub("Ã¢", "â", name_english),
         name_english = gsub("Ã´", "ô", name_english),
         name_english = gsub("Ã\\\u008e", "Î", name_english),
         name_english = gsub("Ã\\\u0089", "É", name_english),
         name_english = gsub("Ã®", "î", name_english))

district_key_2013 %>% print(n = Inf)
