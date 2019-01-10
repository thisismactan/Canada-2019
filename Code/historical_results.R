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

## Demographics
demographics <- fread("Data/Raw/Demographics/districts_sex_age_education_2016.csv") %>%
  as.data.frame() %>%
  as.tbl() %>%
  filter(district_code > 10000, age %in% c("15 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 44 years", "45 to 54 years",
                                           "55 to 64 years", "65 to 74 years", "75 years and over", "Total")) %>%
  mutate(education = case_when(education %in% c("No certificate, diploma or degree",
                                                "Secondary (high) school diploma or equivalency certificate") ~ "HS or less",
                               education %in% c("Apprenticeship or trades certificate or diploma",
                                                "College, CEGEP or other non-university certificate or diploma",
                                                "University certificate or diploma below bachelor level") ~ "College / some university",
                               education %in% c("University certificate, diploma or degree at bachelor level or above") ~ "Bachelor's or above",
                               education %in% c("Bachelor's degree") ~ "Bachelor's only",
                               education %in% c("University certificate, diploma or degree above bachelor level") ~ "Graduate degree",
                               education == "Total" ~ "Total"),
         age = case_when(age %in% c("15 to 24 years", "25 to 29 years") ~ "15 to 29 years",
                         age %in% c("30 to 34 years", "35 to 44 years") ~ "30 to 44 years",
                         age %in% c("45 to 54 years", "55 to 64 years") ~ "45 to 64 years",
                         age %in% c("65 to 74 years", "75 years and over") ~ "65 and older",
                         age == "Total" ~ "Total")) %>%
  group_by(year, district_code, district_name, age, sex, education) %>%
  summarise(total_people = sum(total_people)) %>%
  ungroup()

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
                                 year == 2011 ~ 2011))

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