source("Code/library.R")

#### Scraping Wikipedia page for national polls ####
polls_url <- "https://en.wikipedia.org/wiki/Opinion_polling_in_the_43rd_Canadian_federal_election"

## Get the Wikipedia polls page
polls_GET <- httr::GET(polls_url) %>% 
  httr::content(as = "text", encoding = "UTF-8") %>%
  xml2::read_html()

## Parse into data frame
campaign_header_string <- '//*[@class = "wikitable sortable"]'
national_polls_campaign <- rvest::html_nodes(polls_GET, xpath = header_string)[1] %>%
  # Parse as a table
  rvest::html_table(fill = TRUE) %>%
  
  # Convert to data frame
  as.data.frame() %>%
  
  # Filter out those pesky "Maxime Bernier resigns from..."s
  filter(Polling.firm != Last.dateof.polling.1., Polling.firm != "2015 Election") %>%
  
  # Clean up
  dplyr::select(pollster = 1, last_date = 2, MOE = 10, n = 11, mode = 12, LPC, CPC, NDP, BQ, GPC, PPC = 9) %>%
  replace_na(list(pollster = "", last_date = NA, MOE = "0 pp", n = "", mode = "", LPC = NA, CPC = NA, NDP = NA, BQ = NA, GPC = NA, PPC = NA)) %>%
  mutate(rolling = grepl("\\/", n), 
         last_date = as.Date(last_date, format = "%B %d, %Y"),
         n = str_split(n, " \\(") %>% sapply(head, 1),
         n = gsub(",", "", n),
         MOE = gsub("[[:alpha:]]|[[:space:]]", "", MOE),
         MOE = sub("^.", "", MOE),
         IVR = grepl("IVR", mode),
         online = grepl("online", mode)) %>%
  mutate_at(vars(c("MOE", "n", "LPC", "CPC", "NDP", "BQ", "GPC", "PPC")), as.numeric) %>%
  mutate(PPC = replace(PPC, which(PPC == 0), NA)) %>%
  filter(!is.na(LPC)) %>%
  
  # Estimate MOE from n for polls which have no MOE
  mutate(MOE = case_when(MOE != 0 ~ MOE,
                         (MOE == 0 | is.na(MOE)) ~ 1.3*sqrt(2500/n))) %>%
  
  # Merge in date spreads by pollster
  merge(read_csv("Data/poll_spreads.csv"), by = "pollster", all.x = TRUE) %>%
  
  # Calculate poll age
  mutate(date = last_date - floor(spread/2),
         age = as.numeric(today() - date)) %>%
  
  arrange(age) %>%
  dplyr::select(pollster, date, age, MOE, n, mode, IVR, rolling, LPC, CPC, NDP, BQ, GPC, PPC) %>%
  as.tbl()

national_polls_precampaign <- rvest::html_nodes(polls_GET, xpath = header_string)[2] %>%
  # Parse as a table
  rvest::html_table(fill = TRUE) %>%
  
  # Convert to data frame
  as.data.frame() %>%
  
  # Filter out those pesky "Maxime Bernier resigns from..."s
  filter(Polling.firm != Last.dateof.polling.1., Polling.firm != "2015 Election") %>%
  
  # Clean up
  dplyr::select(pollster = 1, last_date = 2, MOE = 10, n = 11, mode = 12, LPC, CPC, NDP, BQ, GPC, PPC = 9) %>%
  replace_na(list(pollster = "", last_date = NA, MOE = "0 pp", n = "", mode = "", LPC = NA, CPC = NA, NDP = NA, BQ = NA, GPC = NA, PPC = NA)) %>%
  mutate(rolling = grepl("\\/", n), 
         last_date = as.Date(last_date, format = "%B %d, %Y"),
         n = str_split(n, " \\(") %>% sapply(head, 1),
         n = gsub(",", "", n),
         MOE = gsub("[[:alpha:]]|[[:space:]]", "", MOE),
         MOE = sub("^.", "", MOE),
         IVR = grepl("IVR", mode),
         online = grepl("online", mode)) %>%
  mutate_at(vars(c("MOE", "n", "LPC", "CPC", "NDP", "BQ", "GPC", "PPC")), as.numeric) %>%
  mutate(PPC = replace(PPC, which(PPC == 0), NA)) %>%
  filter(!is.na(LPC)) %>%
  
  # Estimate MOE from n for polls which have no MOE
  mutate(MOE = case_when(MOE != 0 ~ MOE,
                         (MOE == 0 | is.na(MOE)) ~ 1.3*sqrt(2500/n))) %>%
  
  # Merge in date spreads by pollster
  merge(read_csv("Data/poll_spreads.csv"), by = "pollster", all.x = TRUE) %>%
  
  # Calculate poll age
  mutate(date = last_date - floor(spread/2),
         age = as.numeric(today() - date)) %>%
  
  arrange(age) %>%
  dplyr::select(pollster, date, age, MOE, n, mode, IVR, rolling, LPC, CPC, NDP, BQ, GPC, PPC) %>%
  as.tbl()

national_polls <- bind_rows(national_polls_campaign, national_polls_precampaign)
