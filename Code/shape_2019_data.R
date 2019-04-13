source("Code/library.R")
source("Code/poll_average.R")

## Assemble the dataset for making 2019 predictions
demographics_2019 <- demographics_2016

results_2019_lag <- results_2015 %>%
  dplyr::select(district_code, LPC_lag = LPC_pct, CPC_lag = CPC_pct, NDP_lag = NDP_pct, Bloc_lag = Bloc_pct, Green_lag = Green_pct,
                winner_last = winner)

results_2015_national <- results_2015 %>%
  summarise_at(vars(ends_with("votes")), sum) %>%
  mutate(LPC_nation_lag = LPC_votes/total_major_votes,
         CPC_nation_lag = CPC_votes/total_major_votes,
         NDP_nation_lag = NDP_votes/total_major_votes,
         Bloc_nation_lag = Bloc_votes/total_major_votes,
         Green_nation_lag = Green_votes/total_major_votes) %>%
  dplyr::select(LPC_nation_lag, CPC_nation_lag, NDP_nation_lag, Bloc_nation_lag, Green_nation_lag) %>%
  dplyr::slice(rep(1, 338))

results_2015_regional <- results_2015 %>%
  mutate(province_code = floor(district_code/1000)) %>%
  left_join(province_key, by = "province_code") %>%
  group_by(region) %>%
  summarise_at(vars(ends_with("votes")), sum) %>%
  mutate(LPC_region_lag = LPC_votes/total_major_votes,
         CPC_region_lag = CPC_votes/total_major_votes,
         NDP_region_lag = NDP_votes/total_major_votes,
         Bloc_region_lag = Bloc_votes/total_major_votes,
         Green_region_lag = Green_votes/total_major_votes) %>%
  dplyr::select(region, LPC_region_lag, CPC_region_lag, NDP_region_lag, Bloc_region_lag, Green_region_lag)

cands_2019 <- read_csv("Data/candidates_2019.csv") %>%
  filter(party %in% c("Liberal", "Conservative", "NDP", "Bloc", "Green", "People's")) %>%
  mutate(candidate = case_when(!is.na(candidate_last) ~ paste(candidate_first, candidate_last),
                               is.na(candidate_last) ~ as.character(NA))) %>%
  dplyr::select(district_code, party, candidate) %>%
  spread(party, candidate) %>%
  dplyr::select(district_code, LPC_cand = Liberal, CPC_cand = Conservative, NDP_cand = NDP, Green_cand = Green, 
                Bloc_cand = Bloc, PPC_cand = `People's`)

national_results_2019 <- national_polls %>% 
  melt(id.vars = c("pollster", "date", "age", "MOE", "n", "mode", "IVR", "weight")) %>%
  filter(variable %in% c("LPC", "CPC", "NDP", "BQ", "GPC", "PPC")) %>%
  group_by(party = variable) %>%
  summarise(average = Hmisc::wtd.mean(value, weights = weight, na.rm = TRUE)/100) %>%
  spread(party, average) %>%
  dplyr::select(LPC_nation = LPC, CPC_nation = CPC, NDP_nation = NDP, Bloc_nation = BQ, Green_nation = GPC)


## How do the frigid northlands lean?
national_results <- bind_rows(national_results_pre2013, national_results_2015)

frigid_northlands_2019 <- (provincial_results_pre2013 %>%
  filter(region == "The frigid northlands") %>%
  bind_rows(regional_results_2015 %>% 
              filter(region == "The frigid northlands") %>% 
              dplyr::select(year, region, LPC_pct = LPC_pct_regional, CPC_pct = CPC_pct_regional, 
                            NDP_pct = NDP_pct_regional, Green_pct = Green_pct_regional, Bloc_pct = Bloc_pct_regional)) %>%
  mutate(LPC_lean = LPC_pct - national_results$LPC_pct_national,
         CPC_lean = CPC_pct - national_results$CPC_pct_national,
         NDP_lean = NDP_pct - national_results$NDP_pct_national,
         Green_lean = Green_pct - national_results$Green_pct_national) %>%
  mutate(weight = 1/(2019 - year)) %>%
  summarise(LPC_region = wtd.mean(LPC_lean, weight),
            CPC_region = wtd.mean(CPC_lean, weight),
            NDP_region = wtd.mean(NDP_lean, weight),
            Green_region = wtd.mean(Green_lean, weight)) %>%
  dplyr::select(-region) +
  (national_results_2019 %>% dplyr::select(-Bloc_nation))) %>%
  mutate(region = "The frigid northlands")

regional_vote <- provincial_polls %>%
  group_by(region = province) %>%
  summarise(LPC_region = wtd.mean(LPC, weight)/100,
            CPC_region = wtd.mean(CPC, weight)/100,
            NDP_region = wtd.mean(NDP, weight)/100,
            Green_region = wtd.mean(GPC, weight)/100,
            Bloc_region = wtd.mean(BQ, weight)/100) %>%
  bind_rows(frigid_northlands_2019) %>%
  mutate(Bloc_region = case_when(is.na(Bloc_region) ~ 0,
                                 !is.na(Bloc_region) ~ Bloc_region))
  
## Simple data (no fundraising)
data_2019.simple <- district_key_2013 %>%
  left_join(results_2019_lag, by = "district_code") %>%
  mutate(province_code = floor(district_code/1000)) %>%
  left_join(province_key, by = "province_code") %>%
  bind_cols(results_2015_national) %>%
  bind_cols(national_results_2019 %>% dplyr::slice(rep(1, 338))) %>%
  left_join(regional_vote, by = "region") %>%
  left_join(cands_2019, by = "district_code") %>%
  left_join(results_2015_regional, by = "region") %>%
  left_join(demographics_2019, by = "district_code") %>%
  left_join(population %>%
              filter(census_year == 2016) %>%
              dplyr::select(district_code, pop_growth_rate),
            by = "district_code") %>%
  mutate(Quebec = province == "Quebec") %>%
  mutate(incumbent = case_when(winner_last == LPC_cand ~ "Liberal",
                               winner_last == CPC_cand ~ "Conservative",
                               winner_last == NDP_cand ~ "NDP",
                               winner_last == Bloc_cand ~ "Bloc",
                               winner_last == Green_cand ~ "Green"),
         incumbent = case_when(is.na(incumbent) ~ "None",
                               !is.na(incumbent) ~ incumbent),
         incumbent = as.factor(incumbent))