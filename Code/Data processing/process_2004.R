source("Code/library.R")

election_year = 2004 # election year
ridings_year = 2003 # year ridings were drawn

## Candidates data
candidates <- read.csv(paste0("Data/candidates_", election_year, ".csv"), stringsAsFactors = FALSE) %>%
  mutate_at(vars(c("candidate_first", "candidate_middle", "candidate_last")), trimws, which = "both") %>%
  mutate(candidate_name = case_when((candidate_middle == "") | is.na(candidate_middle) ~ paste(candidate_first, candidate_last),
                                    candidate_middle != "" ~ paste(candidate_first, candidate_middle, candidate_last))) %>%
  dplyr::select(district_code, candidate_name, party)

## Read in districts data
district_codes <- unique(candidates$district_code)
district_data <- vector("list", length(district_codes))
for(i in 1:length(district_codes)) {
  ## Read in data
  district_data_temp <- readr::read_csv(paste0("Data/Raw/", election_year, " results/pollbypoll", district_codes[i], ".csv"),
                                        locale = locale(encoding = "LATIN1")) %>%
    mutate(district_code = district_codes[i])
  n_fields <- ncol(district_data_temp)
  
  ## Change variable names
  names(district_data_temp)[1:3] <- c("district_name_english_french", "poll_number", "poll_name")
  names(district_data_temp)[(n_fields-3):n_fields] <- c("rejected_votes", "total_votes", "registered_voters", "district_code")
  
  ## Replace candidates with party
  district_data_temp <- district_data_temp  %>%
    mutate(poll_number = 1:n()) %>%
    mutate_at(4:(n_fields-4), as.numeric) %>%
    melt(id.vars = c("district_code", "district_name_english_french", "poll_number", "poll_name",
                     "rejected_votes", "total_votes", "registered_voters"),
         variable.name = "candidate_name", value.name = "votes") %>%
    mutate(votes = as.numeric(votes),
           poll_number = as.character(poll_number),
           candidate_name = as.character(candidate_name)) %>%
    left_join(candidates, by = c("district_code", "candidate_name")) %>%
    dplyr::select(-district_name_english_french, -candidate_name, -rejected_votes) %>%
    filter(party %in% c("Liberal", "Conservative", "NDP", "Bloc", "Green")) %>%
    reshape(v.names = "votes", timevar = "party", idvar = c("district_code", "poll_number", "poll_name"), 
            direction = "wide")
  
  district_data[[i]] <- district_data_temp
}

## Stick them together and merge in riding names
district_key <- read.csv(paste0("Data/electoral_districts_key_", ridings_year, ".csv"), stringsAsFactors = FALSE)
pollresults <- bind_rows(district_data) %>%
  left_join(district_key, by = "district_code") %>%
  mutate(year = election_year) %>%
  dplyr::select(year, district_code, name_english, poll_name, poll_number, Liberal = votes.Liberal, Conservative = votes.Conservative, 
                NDP = votes.NDP, Green = votes.Green, Bloc = votes.Bloc, registered_voters, total_votes, population) %>%
  as.tbl()

write.csv(pollresults, file = paste0("Data/Processed/", election_year, "_results_by_precinct.csv"), row.names = FALSE)
