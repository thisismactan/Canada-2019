#### Obtain national, regional, and riding-level results from historical precinct-level data ####

## Shape data to wide (results_pre2013_wide)
source("Code/shape_data.R")

## National historical results
historical_results.nation <- results_pre2013_wide %>%
  group_by(year) %>%
  summarise_at(vars(c("LPC", "CPC", "NDP", "Green", "Bloc", "total")), sum) %>%
  ungroup() %>%
  mutate(LPC_nation = LPC/total,
         CPC_nation = CPC/total,
         NDP_nation = NDP/total,
         Green_nation = Green/total,
         Bloc_nation = Bloc/total) %>%
  arrange(year) %>%
  mutate(LPC_nation_lag = lag(LPC_nation),
         CPC_nation_lag = lag(CPC_nation),
         NDP_nation_lag = lag(NDP_nation),
         Green_nation_lag = lag(Green_nation),
         Bloc_nation_lag = lag(Bloc_nation)) %>%
  ungroup()

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
         Bloc_region = Bloc/total) %>%
  arrange(region, year) %>%
  group_by(region) %>%
  mutate(LPC_region_lag = lag(LPC_region),
         CPC_region_lag = lag(CPC_region),
         NDP_region_lag = lag(NDP_region),
         Green_region_lag = lag(Green_region),
         Bloc_region_lag = lag(Bloc_region)) %>%
  ungroup()

## Estimate demographics by year
demographics_2001 <- read_csv("Data/Processed/2001_demographics.csv") %>%
  mutate(year = 2001)
demographics_2006 <- read_csv("Data/Processed/2006_demographics.csv") %>%
  mutate(year = 2006)
demographics_2011 <- read_csv("Data/Processed/2011_demographics.csv") %>%
  mutate(year = 2011)
demographics_2011_2013_order <- read_csv("Data/Processed/2011_demographics_2013_order.csv") %>%
  mutate(year = 2011)
demographics_2016 <- read_csv("Data/Processed/2016_demographics.csv") %>%
  mutate(year = 2016)

demographics_2004 <- as.tbl(((1/(2004-2001))*demographics_2001 + (1/(2006-2004))*demographics_2006)/(1/(2004-2001) + 1/(2006-2004))) %>%
  mutate_at(vars(c("year", "district_code")), as.integer)
demographics_2008 <- as.tbl(((1/(2008-2006))*demographics_2006 + (1/(2011-2008))*demographics_2011)/(1/(2008-2006) + 1/(2011-2008))) %>%
  mutate_at(vars(c("year", "district_code")), as.integer)
demographics_2015 <- as.tbl(((1/(2015-2011))*demographics_2011_2013_order + (1/(2016-2015))*demographics_2016)/(1/(2015-2011) + 1/(2016-2015))) %>%
  mutate_at(vars(c("year", "district_code")), as.integer)

demographics_pre2013 <- bind_rows(demographics_2004, demographics_2006, demographics_2008, demographics_2011)

## District historical results
historical_results.district <- results_pre2013_wide %>%
  mutate(LPC = LPC/total,
         CPC = CPC/total,
         NDP = NDP/total,
         Bloc = Bloc/total,
         Green = Green/total,
         province_code = floor(district_code/1000)) %>%
  left_join(province_key, by = "province_code") %>%
  arrange(district_code, year) %>%
  group_by(district_code) %>%
  mutate(Bloc_lag = lag(Bloc),
         CPC_lag = lag(CPC),
         Green_lag = lag(Green),
         LPC_lag = lag(LPC),
         NDP_lag = lag(NDP)) %>%
  left_join(historical_results.region %>% dplyr::select(-LPC, -CPC, -NDP, -Green, -Bloc, -total), by = c("region", "year")) %>%
  left_join(historical_results.nation %>% dplyr::select(-LPC, -CPC, -NDP, -Green, -Bloc, -total), by = "year") %>%
  mutate(LPC_change = LPC - LPC_lag,
         CPC_change = CPC - CPC_lag,
         NDP_change = NDP - NDP_lag,
         Green_change = Green - Green_lag,
         Bloc_change = Bloc - Bloc_lag) %>%
  ungroup() %>%
  mutate(incumbent = factor(incumbent),
         census_year = case_when(year %in% c(2004, 2006, 2008) ~ 2006,
                                 year == 2011 ~ 2011),
         LPC_funds_frac = LPC_funds/total_funds,
         CPC_funds_frac = CPC_funds/total_funds,
         NDP_funds_frac = NDP_funds/total_funds,
         Bloc_funds_frac = Bloc_funds/total_funds,
         Green_funds_frac = Green_funds/total_funds) %>%
  left_join(population, by = c("district_code", "census_year")) %>%
  mutate(years_since_census = year - census_year,
         pop_growth = pop_growth_rate^years_since_census) %>%
  left_join(demographics_pre2013, by = c("district_code", "year"))

## Convert to logit
historical_results.logit <- historical_results.district %>%
  mutate_at(vars(c("Bloc", "CPC", "Green", "LPC", "NDP", "Bloc_lag", "CPC_lag", "Green_lag", "LPC_lag", "NDP_lag", "LPC_region", "CPC_region",
                   "NDP_region", "Green_region", "Bloc_region", "LPC_nation", "CPC_nation", "NDP_nation", "Green_nation", "Bloc_nation")),
            logit) %>%
  mutate_at(vars(c("Bloc", "CPC", "Green", "LPC", "NDP", "Bloc_lag", "CPC_lag", "Green_lag", "LPC_lag", "NDP_lag", "LPC_region", "CPC_region",
                   "NDP_region", "Green_region", "Bloc_region", "LPC_nation", "CPC_nation", "NDP_nation", "Green_nation", "Bloc_nation")),
            function(x) ifelse(x == -Inf, NA, x)) %>%
  mutate(LPC_change = LPC - LPC_lag,
         CPC_change = CPC - CPC_lag,
         NDP_change = NDP - NDP_lag,
         Green_change = Green - Green_lag,
         Bloc_change = Bloc - Bloc_lag) %>%
  ungroup()

## By year
results_2006 <- historical_results.district %>%
  filter(year == 2006)

results_2008 <- historical_results.district %>%
  filter(year == 2008)

results_2011 <- historical_results.district %>%
  filter(year == 2011)

results_2006.logit <- historical_results.logit %>%
  filter(year == 2006)

results_2008.logit <- historical_results.logit %>%
  filter(year == 2008)

results_2011.logit <- historical_results.logit %>%
  filter(year == 2011)