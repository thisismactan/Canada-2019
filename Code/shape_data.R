library(tidyverse)

#### Data shaping ####
## Read in election results and candidates
results_2004 <- read.csv("Data/Processed/2004_results_by_precinct.csv", stringsAsFactors = FALSE) %>%
  mutate(Liberal_total = sum(Liberal, na.rm = TRUE),
         Conservative_total = sum(Conservative, na.rm = TRUE),
         NDP_total = sum(NDP, na.rm = TRUE),
         Green_total = sum(Green, na.rm = TRUE),
         Bloc_total = sum(Bloc, na.rm = TRUE))
results_2006 <- read.csv("Data/Processed/2006_results_by_precinct.csv", stringsAsFactors = FALSE) %>%
  mutate(Liberal_total = sum(Liberal, na.rm = TRUE),
         Conservative_total = sum(Conservative, na.rm = TRUE),
         NDP_total = sum(NDP, na.rm = TRUE),
         Green_total = sum(Green, na.rm = TRUE),
         Bloc_total = sum(Bloc, na.rm = TRUE))
results_2008 <- read.csv("Data/Processed/2008_results_by_precinct.csv", stringsAsFactors = FALSE) %>%
  mutate(Liberal_total = sum(Liberal, na.rm = TRUE),
         Conservative_total = sum(Conservative, na.rm = TRUE),
         NDP_total = sum(NDP, na.rm = TRUE),
         Green_total = sum(Green, na.rm = TRUE),
         Bloc_total = sum(Bloc, na.rm = TRUE))
results_2011 <- read.csv("Data/Processed/2011_results_by_precinct.csv", stringsAsFactors = FALSE) %>%
  mutate(Liberal_total = sum(Liberal, na.rm = TRUE),
         Conservative_total = sum(Conservative, na.rm = TRUE),
         NDP_total = sum(NDP, na.rm = TRUE),
         Green_total = sum(Green, na.rm = TRUE),
         Bloc_total = sum(Bloc, na.rm = TRUE))
results_2015 <- read.csv("Data/Processed/2015_results_by_precinct.csv", stringsAsFactors = FALSE) %>%
  mutate(Liberal_total = sum(Liberal, na.rm = TRUE),
         Conservative_total = sum(Conservative, na.rm = TRUE),
         NDP_total = sum(NDP, na.rm = TRUE),
         Green_total = sum(Green, na.rm = TRUE),
         Bloc_total = sum(Bloc, na.rm = TRUE))

cands_2004 <- read.csv("Data/candidates_2004.csv", stringsAsFactors = FALSE) %>% mutate(year = 2004)
cands_2006 <- read.csv("Data/candidates_2006.csv", stringsAsFactors = FALSE) %>% mutate(year = 2006)
cands_2008 <- read.csv("Data/candidates_2008.csv", stringsAsFactors = FALSE) %>% mutate(year = 2008)
cands_2011 <- read.csv("Data/candidates_2011.csv", stringsAsFactors = FALSE) %>% mutate(year = 2011)
cands_2015 <- read.csv("Data/candidates_2015.csv", stringsAsFactors = FALSE) %>% mutate(year = 2015)

## Reshaping to district-year level
cands_pre2013 <- bind_rows(cands_2004, cands_2006, cands_2008, cands_2011) %>%
  filter(party %in% c("Liberal", "Conservative", "NDP", "Green", "Bloc")) %>%
  as.tbl() %>%
  mutate(candidate = case_when((candidate_middle == "") | is.na(candidate_middle) ~ paste(candidate_first, candidate_last),
                               candidate_middle != "" ~ paste(candidate_first, candidate_middle, candidate_last))) %>%
  dplyr::select(district_code, year, party, candidate) %>%
  spread(party, candidate)

## Identifying incumbents
results_pre2013 <- bind_rows(results_2004, results_2006, results_2008, results_2011) %>%
  as.tbl() %>%
  group_by(district_code, name_english, year) %>%
  summarise_at(vars(c("Liberal", "Conservative", "NDP", "Green", "Bloc", "rejected_votes", "total_votes", "registered_voters")), sum, na.rm = TRUE) %>%
  left_join(cands_pre2013, by = c("district_code", "year")) %>%
  select(district_code, name_english, year, LPC_cand = Liberal.y, CPC_cand = Conservative.y, NDP_cand = NDP.y, Green_cand = Green.y, Bloc_cand = Bloc.y, 
         registered_voters, total_votes, LPC_votes = Liberal.x, CPC_votes = Conservative.x, NDP_votes = NDP.x, Green_votes = Green.x, 
         Bloc_votes = Bloc.x) %>%
  ungroup() %>%
  mutate(total_major_votes = LPC_votes + CPC_votes + NDP_votes + Green_votes + Bloc_votes,
         LPC_pct = LPC_votes/total_major_votes,
         CPC_pct = CPC_votes/total_major_votes,
         NDP_pct = NDP_votes/total_major_votes,
         Green_pct = Green_votes/total_major_votes,
         Bloc_pct = Bloc_votes/total_major_votes,
         max_pct = pmax(LPC_pct, CPC_pct, NDP_pct, Green_pct, Bloc_pct),
         winner = case_when(LPC_pct == max_pct ~ LPC_cand,
                            CPC_pct == max_pct ~ CPC_cand,
                            NDP_pct == max_pct ~ NDP_cand,
                            Green_pct == max_pct ~ Green_cand,
                            Bloc_pct == max_pct ~ Bloc_cand) %>% as.character()) %>%
  group_by(district_code) %>%
  mutate_at(vars(c("winner", "LPC_pct", "CPC_pct", "NDP_pct", "Green_pct", "Bloc_pct")),
            funs(last = lag)) %>%
  ungroup() %>% 
  mutate(incumbent = case_when(winner_last == LPC_cand ~ "Liberal",
                               winner_last == CPC_cand ~ "Conservative",
                               winner_last == NDP_cand ~ "NDP",
                               winner_last == Green_cand ~ "Green",
                               winner_last == Bloc_cand ~ "Bloc"),
         incumbent = case_when(!is.na(incumbent) ~ incumbent,
                               is.na(incumbent) ~ "None") %>% as.character())
