source("Code/library.R")

#### District/province/region key ####
province_key <- tibble(province_code = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 60, 61, 62),
                       province = c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick", "Quebec",
                                    "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Yukon", "Northwest Territories",
                                    "Nunavut"),
                       province_abbr = c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU"),
                       region = c("Atlantic", "Atlantic", "Atlantic", "Atlantic", "Quebec", "Ontario", "Prairie", "Prairie", "Alberta",
                                  "British Columbia", "The frigid northlands", "The frigid northlands", "The frigid northlands"))

## District keys
source("Code/Data processing/clean_district_keys.R")

#### Data shaping ####
## Read in election results and candidates
results_2004 <- read.csv("Data/Processed/2004_results_by_precinct.csv") %>%
  mutate(Liberal_total = sum(Liberal, na.rm = TRUE),
         Conservative_total = sum(Conservative, na.rm = TRUE),
         NDP_total = sum(NDP, na.rm = TRUE),
         Green_total = sum(Green, na.rm = TRUE),
         Bloc_total = sum(as.numeric(Bloc), na.rm = TRUE)) %>%
  mutate(province_code = round(district_code/1000)) %>%
  left_join(province_key, by = "province_code")
results_2006 <- read.csv("Data/Processed/2006_results_by_precinct.csv") %>%
  mutate(Liberal_total = sum(Liberal, na.rm = TRUE),
         Conservative_total = sum(Conservative, na.rm = TRUE),
         NDP_total = sum(NDP, na.rm = TRUE),
         Green_total = sum(Green, na.rm = TRUE),
         Bloc_total = sum(as.numeric(Bloc), na.rm = TRUE)) %>%
  mutate(province_code = round(district_code/1000)) %>%
  left_join(province_key, by = "province_code")
results_2008 <- read.csv("Data/Processed/2008_results_by_precinct.csv") %>%
  mutate(Liberal_total = sum(Liberal, na.rm = TRUE),
         Conservative_total = sum(Conservative, na.rm = TRUE),
         NDP_total = sum(NDP, na.rm = TRUE),
         Green_total = sum(Green, na.rm = TRUE),
         Bloc_total = sum(as.numeric(Bloc), na.rm = TRUE)) %>%
  mutate(province_code = round(district_code/1000)) %>%
  left_join(province_key, by = "province_code")
results_2011 <- read.csv("Data/Processed/2011_results_by_precinct.csv") %>%
  mutate(Liberal_total = sum(Liberal, na.rm = TRUE),
         Conservative_total = sum(Conservative, na.rm = TRUE),
         NDP_total = sum(NDP, na.rm = TRUE),
         Green_total = sum(Green, na.rm = TRUE),
         Bloc_total = sum(as.numeric(Bloc), na.rm = TRUE)) %>%
  mutate(province_code = round(district_code/1000)) %>%
  left_join(province_key, by = "province_code")

cands_2004 <- fread_to_tbl("Data/candidates_2004.csv") %>% mutate(year = 2004)
cands_2006 <- fread_to_tbl("Data/candidates_2006.csv") %>% mutate(year = 2006)
cands_2008 <- fread_to_tbl("Data/candidates_2008.csv") %>% mutate(year = 2008)
cands_2011 <- fread_to_tbl("Data/candidates_2011.csv") %>% mutate(year = 2011)

## Reshaping to district-year level
cands_pre2013 <- bind_rows(cands_2004, cands_2006, cands_2008, cands_2011) %>%
  filter(party %in% c("Liberal", "Conservative", "NDP", "Green", "Bloc")) %>%
  as.tbl() %>%
  mutate(candidate_first = sapply(str_split(candidate_first, "[[:space:]]"), head, n = 1),
         candidate = case_when((candidate_middle == "") | is.na(candidate_middle) ~ paste(candidate_first, candidate_last),
                               candidate_middle != "" ~ paste(candidate_first, candidate_middle, candidate_last))) %>%
  dplyr::select(district_code, year, party, candidate) %>%
  spread(party, candidate)

incumbents_pre2013 <- bind_rows(read_csv("Data/incumbents_2006.csv") %>% mutate(year = 2006),
                                read_csv("Data/incumbents_2008.csv") %>% mutate(year = 2008),
                                read_csv("Data/incumbents_2011.csv") %>% mutate(year = 2011)) %>%
  dplyr::select(-population, -name_english)

cands_2015 <- fread_to_tbl("Data/candidates_2015.csv") %>% 
  mutate(year = 2015) %>%
  filter(party %in% c("Liberal", "Conservative", "NDP", "Green", "Bloc")) %>%
  as.tbl() %>%
  mutate(candidate_first = str_split(candidate_first, " ") %>% sapply(head, n = 1),
         candidate = paste(candidate_first, candidate_last)) %>%
  dplyr::select(district_code, year, party, candidate) %>%
  spread(party, candidate)

## Stitching on campaign finance numbers
contribs_2004_2011 <- fread_to_tbl("Data/Processed/campaign_contributions_2004_2011.csv")

## Identifying incumbents
results_pre2013 <- bind_rows(results_2004, results_2006, results_2008, results_2011) %>%
  as.tbl() %>%
  group_by(district_code, name_english, year) %>%
  mutate_at(vars(c("Liberal", "Conservative", "NDP", "Green", "Bloc", "total_votes", "registered_voters")), as.numeric) %>%
  summarise_at(vars(c("Liberal", "Conservative", "NDP", "Green", "Bloc", "total_votes", "registered_voters")), sum, na.rm = TRUE) %>% 
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
  merge(incumbents_pre2013, by = c("district_code", "year"), all.x = TRUE) %>%
  as.tbl() %>%
  mutate(incumbent = case_when(last_winner == "Liberal" & incumbent_running ~ "Liberal",
                               last_winner == "Conservative" & incumbent_running ~ "Conservative",
                               last_winner == "NDP" & incumbent_running ~ "NDP",
                               last_winner == "Green" & incumbent_running ~ "Green",
                               last_winner == "Bloc" & incumbent_running ~ "Bloc",
                               !incumbent_running ~ "None"))

## Long format
results_pre2013_long <- results_pre2013 %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent")) %>%
  mutate(variable = as.character(variable),
         party = str_split(variable, "_") %>% sapply(head, n = 1),
         variable = sub(".*_", "", variable)) %>%
  as.tbl() %>%
  left_join(contribs_2004_2011, by = c("district_code", "year")) %>%
  dplyr::select(district_code, name_english = name_english.y, year, incumbent, variable, value, party, LPC_funds, 
                CPC_funds, NDP_funds, Green_funds, Bloc_funds, total_funds)

## Wide format
results_pre2013_wide <- results_pre2013_long %>%
  filter(variable == "votes") %>%
  mutate(votes = as.numeric(value)) %>%
  dplyr::select(-variable, -value) %>%
  group_by(district_code, name_english, year) %>%
  distinct(district_code, name_english, year, party, incumbent, .keep_all = TRUE) %>%
  spread(party, votes)

## National results
national_results_pre2013 <- results_pre2013 %>%
  group_by(year) %>%
  summarise_at(vars(c("LPC_votes", "CPC_votes", "NDP_votes", "Green_votes", "Bloc_votes")), sum) %>%
  mutate(total = LPC_votes + CPC_votes + NDP_votes + Green_votes + Bloc_votes) %>%
  mutate(LPC_pct_national = LPC_votes/total,
         CPC_pct_national = CPC_votes/total,
         NDP_pct_national = NDP_votes/total,
         Green_pct_national = Green_votes/total) %>%
  dplyr::select(year, LPC_pct_national, CPC_pct_national, NDP_pct_national, Green_pct_national)

## Regional results
regional_results_pre2013 <- results_pre2013 %>%
  mutate(province_code = floor(district_code/1000)) %>%
  left_join(province_key, by = "province_code") %>%
  group_by(region, province_code, year) %>%
  summarise_at(vars(c("LPC_votes", "CPC_votes", "NDP_votes", "Green_votes", "Bloc_votes")), sum) %>%
  mutate(total = LPC_votes + CPC_votes + NDP_votes + Green_votes + Bloc_votes) %>%
  mutate(LPC_pct_regional = LPC_votes/total,
         CPC_pct_regional = CPC_votes/total,
         NDP_pct_regional = NDP_votes/total,
         Green_pct_regional = Green_votes/total,
         Bloc_pct_regional = Bloc_votes/total) %>%
  dplyr::select(year, province_code, region, LPC_pct_regional, CPC_pct_regional, NDP_pct_regional, Green_pct_regional, Bloc_pct_regional)


#### Now the same for 2015 ####
## Identifying incumbents
results_2015 <- read.csv("Data/Processed/2015_results_by_precinct.csv") %>%
  mutate(Liberal_total = sum(Liberal, na.rm = TRUE),
         Conservative_total = sum(Conservative, na.rm = TRUE),
         NDP_total = sum(NDP, na.rm = TRUE),
         Green_total = sum(Green, na.rm = TRUE),
         Bloc_total = sum(as.numeric(Bloc), na.rm = TRUE)) %>%
  mutate(province_code = round(district_code/1000)) %>%
  left_join(province_key, by = "province_code") %>%
  as.tbl() %>%
  group_by(district_code, name_english, year) %>%
  mutate_at(vars(c("Liberal", "Conservative", "NDP", "Green", "Bloc", "total_votes", "registered_voters")), as.numeric) %>%
  summarise_at(vars(c("Liberal", "Conservative", "NDP", "Green", "Bloc", "total_votes", "registered_voters")), sum, na.rm = TRUE) %>%
  left_join(cands_2015, by = c("district_code", "year")) %>%
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

## Long format
results_2015_long <- results_2015 %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent")) %>%
  mutate(variable = as.character(variable),
         party = str_split(variable, "_") %>% sapply(head, n = 1),
         variable = sub(".*_", "", variable),
         merge_name = name_english) %>%
  as.tbl() %>%
  dplyr::select(district_code, district = merge_name, year, incumbent, variable, value, party)

## Wide format
results_2015_wide <- results_2015_long %>%
  filter(variable == "votes") %>%
  mutate(votes = as.numeric(value)) %>%
  dplyr::select(-variable, -value) %>%
  group_by(district_code, name_english = district, year) %>%
  distinct(district_code, name_english, year, party, incumbent, .keep_all = TRUE) %>%
  spread(party, votes)

## National results
national_results_2015 <- results_2015 %>%
  group_by(year) %>%
  summarise_at(vars(c("LPC_votes", "CPC_votes", "NDP_votes", "Green_votes", "Bloc_votes")), sum) %>%
  mutate(total = LPC_votes + CPC_votes + NDP_votes + Green_votes + Bloc_votes) %>%
  mutate(LPC_pct_national = LPC_votes/total,
         CPC_pct_national = CPC_votes/total,
         NDP_pct_national = NDP_votes/total,
         Green_pct_national = Green_votes/total) %>%
  dplyr::select(year, LPC_pct_national, CPC_pct_national, NDP_pct_national, Green_pct_national)

## Regional results
regional_results_2015 <- results_2015 %>%
  mutate(province_code = floor(district_code/1000)) %>%
  left_join(province_key, by = "province_code") %>%
  group_by(region, year) %>%
  summarise_at(vars(c("LPC_votes", "CPC_votes", "NDP_votes", "Green_votes", "Bloc_votes")), sum) %>%
  mutate(total = LPC_votes + CPC_votes + NDP_votes + Green_votes + Bloc_votes) %>%
  mutate(LPC_pct_regional = LPC_votes/total,
         CPC_pct_regional = CPC_votes/total,
         NDP_pct_regional = NDP_votes/total,
         Green_pct_regional = Green_votes/total,
         Bloc_pct_regional = Bloc_votes/total) %>%
  dplyr::select(year, region, LPC_pct_regional, CPC_pct_regional, NDP_pct_regional, Green_pct_regional, Bloc_pct_regional)
