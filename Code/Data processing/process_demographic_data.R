#### Processing demographic data ####
source("Code/library.R")

#### Population counts ####
pop2006 <- fread_to_tbl("Data/Raw/Demographics/districts_pop_2006.csv") %>%
  mutate(census_year = 2006, census_year_lag = 2001)
pop2011 <- fread_to_tbl("Data/Raw/Demographics/districts_pop_2011.csv") %>%
  mutate(census_year = 2011, census_year_lag = 2006)
pop2016 <- fread_to_tbl("Data/Raw/Demographics/districts_pop_2016.csv") %>%
  mutate(census_year = 2016, census_year_lag = 2011)

population <- bind_rows(pop2006, pop2011, pop2016) %>%
  dplyr::select(census_year, census_year_lag, district_code, pop, pop_lag, dwellings, occupied_dwellings) %>%
  mutate(pop_change = pop/pop_lag,
         pop_growth_rate = (pop_change)^(1/(census_year - census_year_lag)) - 1)

#### Age, sex, and education for 2016 ####
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