#### Obtain national, regional, and riding-level results from historical precinct-level data ####

## Shape data to wide (results_pre2013_wide)
source("Code/shape_data.R")

## National historical results
historical_results.nation <- results_pre2013_wide.votes %>%
  group_by(year) %>%
  summarise_at(vars(c("LPC", "CPC", "NDP", "Green", "Bloc", "total")), sum) %>%
  ungroup() %>%
  mutate(LPC_nation = LPC/total,
         CPC_nation = CPC/total,
         NDP_nation = NDP/total,
         Green_nation = Green/total,
         Bloc_nation = Bloc/total)

## Regional historical results

# Seats
historical_results.region_seats <- results_pre2013_wide %>%
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
historical_results.region <- results_pre2013_wide %>%
  mutate(province_code = floor(district_code/1000)) %>%
  left_join(province_key, by = "province_code") %>%
  group_by(region, year) %>%
  summarise_at(vars(c("LPC", "CPC", "NDP", "Green", "Bloc", "total")), sum) %>%
  ungroup() %>%
  mutate(LPC_region = LPC/total,
         CPC_region = CPC/total,
         NDP_region = NDP/total,
         Green_region = Green/total,
         Bloc_region = Bloc/total)

## District historical results
historical_results.district <- results_pre2013_wide %>%
  mutate(LPC = LPC/total,
         CPC = CPC/total,
         NDP = NDP/total,
         Bloc = Bloc/total,
         Green = Green/total,
         province_code = floor(district_code/1000)) %>%
  left_join(province_key, by = "province_code") %>%
  group_by(district_code) %>%
  mutate(Bloc_lag = lag(Bloc),
         CPC_lag = lag(CPC),
         Green_lag = lag(Green),
         LPC_lag = lag(LPC),
         NDP_lag = lag(NDP)) %>%
  left_join(historical_results.region %>% dplyr::select(-LPC, -CPC, -NDP, -Green, -Bloc, -total), by = c("region", "year")) %>%
  left_join(historical_results.nation %>% dplyr::select(-LPC, -CPC, -NDP, -Green, -Bloc, -total), by = "year")

## Define (inverse) logit function
logit <- function(x) {
  x <- log(x/(1-x))
  return(x)
}

invlogit <- function(x) {
  x <- exp(x)/(1 + exp(x))
  return(x)
}

## Convert to logit
historical_results.logit <- historical_results.district %>%
  mutate_at(vars(c("Bloc", "CPC", "Green", "LPC", "NDP", "Bloc_lag", "CPC_lag", "Green_lag", "LPC_lag", "NDP_lag", "LPC_region", "CPC_region",
                   "NDP_region", "Green_region", "Bloc_region", "LPC_nation", "CPC_nation", "NDP_nation", "Green_nation", "Bloc_nation")),
            logit) %>%
  mutate_at(vars(c("Bloc", "CPC", "Green", "LPC", "NDP", "Bloc_lag", "CPC_lag", "Green_lag", "LPC_lag", "NDP_lag", "LPC_region", "CPC_region",
                   "NDP_region", "Green_region", "Bloc_region", "LPC_nation", "CPC_nation", "NDP_nation", "Green_nation", "Bloc_nation")),
            function(x) ifelse(x == -Inf, NA, x))
