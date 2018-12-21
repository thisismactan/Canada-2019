#### Scraping Wikipedia page for polls ####
library(httr)
library(rvest)
library(tidyverse)
library(xml2)

polls_url <- "https://en.wikipedia.org/wiki/Opinion_polling_in_the_43rd_Canadian_federal_election"

## Get the Wikipedia polls page
polls_GET <- httr::GET(polls_url) %>% 
  httr::content(as = "text", encoding = "UTF-8") %>%
  xml2::read_html()

## Parse into data frame
header_string <- '//*[@class = "wikitable sortable"]'
poll_data <- rvest::html_nodes(polls_html, xpath = header_string)[1] %>%
  # Parse as a table
  rvest::html_table() %>%
  
  # Convert to tibble
  as.data.frame() %>% as.tbl() %>%
  
  # Filter out those pesky "Maxime Bernier resigns from..."s
  filter(Polling.firm != Last.dateof.polling, Polling.firm != "2015 Election") %>%
  
  # Clean up
  dplyr::select(pollster = Polling.firm, last_date = Last.dateof.polling, MOE = Marginof.error.1.,
                mode = Polling.method.3., LPC, CPC, NDP, BQ, GPC, PPC) %>%
  replace_na(list(pollster = "", last_date = NA, MOE = "0 pp", mode = "", LPC = NA, CPC = NA, NDP = NA, BQ = NA, GPC = NA, PPC = NA)) %>%
  mutate(last_date = as.Date(last_date, format = "%B %d, %Y"),
         MOE = gsub("[[:alpha:]]|[[:space:]]", "", MOE),
         MOE = sub("^.", "", MOE)) %>%
  mutate_at(vars(c("MOE", "LPC", "CPC", "NDP", "BQ", "GPC", "PPC")), as.numeric)
  
