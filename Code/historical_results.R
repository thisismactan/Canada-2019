#### Obtain national, regional, and riding-level results from historical precinct-level data ####

## Shape data to wide (results_pre2013_wide)
source("Code/shape_data.R")

## National historical results
historical_results.nation <- results_pre2013_wide.votes %>%
  group_by(year) %>%
  summarise_at(vars(c("LPC", "CPC", "NDP", "Green", "total")), sum) %>%
  ungroup() %>%
  mutate(LPC_pct = LPC/total,
         CPC_pct = CPC/total,
         NDP_pct = NDP/total,
         Green_pct = Green/total)

## Regional historical results

# Seats
historical_results.region_seats <- results_pre2013_wide.votes %>%
  mutate(max = pmax(Bloc, CPC, Green, LPC, NDP),
         winner = case_when(Bloc == max ~ "Bloc",
                            CPC == max ~ "CPC", 
                            LPC == max ~ "LPC", 
                            NDP == max ~ "NDP",
                            Green == max ~ "Green")) %>%
  mutate(province_code = floor(district_code/1000)) %>%
  left_join(province_key, by = "province_code") %>%
  group_by(region, year, winner) %>%
  summarise(seats = n()) %>%
  spread(winner, seats, fill = 0) %>%
  rename(LPC_seats = LPC, CPC_seats = CPC, NDP_seats = NDP, Green_seats = Green, Bloc_seats = Bloc)

# Votes
historical_results.region <- results_pre2013_wide.votes %>%
  mutate(province_code = floor(district_code/1000)) %>%
  left_join(province_key, by = "province_code") %>%
  group_by(region, year) %>%
  summarise_at(vars(c("LPC", "CPC", "NDP", "Green", "Bloc", "total")), sum) %>%
  ungroup() %>%
  mutate(LPC_pct = LPC/total,
         CPC_pct = CPC/total,
         NDP_pct = NDP/total,
         Green_pct = Green/total,
         Bloc_pct = Bloc/total)

## District historical results
historical_results.district <- results_pre2013_wide %>%
  mutate(province_code = floor(district_code/1000)) %>%
  left_join(province_key, by = "province_code") %>%
  group_by(district_code) %>%
  mutate(Bloc_lag = lag(Bloc),
         CPC_lag = lag(CPC),
         Green_lag = lag(Green),
         LPC_lag = lag(LPC),
         NDP_lag = lag(NDP)) %>%
  left_join(historical_results.region %>% dplyr::select(-LPC, -CPC, -NDP, -Green, -Bloc, -total), by = c("region", "year"))
