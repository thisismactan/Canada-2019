#### Obtain national and regional-level results from historical precinct-level data ####

## Shape data to wide (results_pre2013_wide)
source("Code/shape_data.R")

historical_results <- results_pre2013_wide %>%
  group_by(district_code, name_english) %>%
  mutate(Bloc_lag = lag(Bloc),
         CPC_lag = lag(CPC),
         Green_lag = lag(Green),
         LPC_lag = lag(LPC),
         NDP_lag = lag(NDP))