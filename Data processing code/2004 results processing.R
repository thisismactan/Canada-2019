election_year = 2004 # election year
ridings_year = 2003 # year ridings were drawn

library(reshape2)
library(tidyverse)

## Candidates data
candidates <- read.csv(paste0("Data/candidates_", election_year, ".csv"), stringsAsFactors = FALSE) %>%
  mutate(candidate_name = case_when(candidate_middle == "" ~ paste(candidate_first, candidate_last),
                                    candidate_middle != "" ~ paste(candidate_first, candidate_middle, candidate_last)),
         candidate_name = gsub("[[:punct:]]", ".", candidate_name),
         candidate_name = gsub("[[:blank:]]", ".", candidate_name)) %>%
  dplyr::select(district_code, candidate_name, party)

## Read in districts data
district_codes <- unique(candidates$district_code)
district_data <- vector("list", length(district_codes))
for(i in 1:length(district_codes)) {
  ## Read in data
  district_data_temp <- read.csv(paste0("Data/Raw/", election_year, " results/pollbypoll", district_codes[i], ".csv"),
                                 header = TRUE, stringsAsFactors = FALSE) %>%
    mutate(district_code = district_codes[i])
  n_fields <- ncol(district_data_temp)
  
  ## Change variable names
  names(district_data_temp)[1:3] <- c("district_name_english_french", "poll_number", "poll_name")
  names(district_data_temp)[(n_fields-3):n_fields] <- c("rejected_votes", "total_votes", "registered_voters", "district_code")
  
  ## Replace candidates with party
  district_data_temp <- district_data_temp %>%
    melt(id.vars = c("district_code", "district_name_english_french", "poll_number", "poll_name",
                     "rejected_votes", "total_votes", "registered_voters"),
         variable.name = "candidate_name", value.name = "votes") %>%
    mutate(votes = as.numeric(votes),
           poll_number = as.character(poll_number),
           candidate_name = as.character(candidate_name)) %>%
    left_join(candidates, by = c("district_code", "candidate_name")) %>%
    dplyr::select(-district_name_english_french, -candidate_name) %>%
    filter(party %in% c("Liberal", "Conservative", "NDP", "Bloc", "Green")) %>%
    reshape(v.names = "votes", timevar = "party", idvar = c("district_code", "poll_number", "poll_name", 
                                                            "rejected_votes", "total_votes", "registered_voters"), 
            direction = "wide")
  
  district_data[[i]] <- district_data_temp
}

## Stick them together and merge in riding names
district_key <- read.csv(paste0("Data/electoral_districts_key_", ridings_year, ".csv"), stringsAsFactors = FALSE)
pollresults <- bind_rows(district_data) %>%
  left_join(district_key, by = "district_code") %>%
  dplyr::select(district_code, name_english, poll_name, poll_number, Liberal = votes.Liberal, Conservative = votes.Conservative, 
                NDP = votes.NDP, Green = votes.Green, Bloc = votes.Bloc, rejected_votes, registered_voters, total_votes, population) %>%
  as.tbl()

write.csv(pollresults, file = paste0("Data/Processed/", election_year, "_results_by_precinct.csv"))
