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

#### Age, sex, education, and race for 2016 ####
demographics_2016_raw <- fread_to_tbl("Data/Raw/Demographics/district_profiles_2016.csv")

names(demographics_2016_raw) <- c("year", "district_code", "geo_level", "name_english", "gnr", "gnr_lf", "data_quality_flag", "alt_district_code", 
                                  "characteristic", "characteristic_id", "notes_profile", "pop", "male_pop", "female_pop")

demographics_2016 <- demographics_2016_raw %>%
  filter(district_code > 10000 & district_code < 99999,
         characteristic_id %in% c(6, 8, 14:24, 695:705, 1324, 1337, 1684:1685, 1687, 1690:1692)) %>%
  dplyr::select(-notes_profile) %>%
  mutate_at(vars(c("pop", "male_pop", "female_pop")), as.numeric) 
  
age_2016 <- demographics_2016 %>%
  mutate(age = case_when(characteristic_id %in% 14:16 ~ "age_1529",
                         characteristic_id %in% 17:19 ~ "age_3044",
                         characteristic_id %in% 20:23 ~ "age_4564",
                         characteristic_id == 24 ~ "age_65")) %>%
  na.omit() %>%
  group_by(district_code, age) %>%
  summarise(pop = sum(pop)) %>%
  mutate(pct = pop/sum(pop)) %>%
  dplyr::select(district_code, age, pct) %>%
  spread(age, pct)

education_2016 <- demographics_2016 %>%
  mutate(education = case_when(characteristic_id %in% 1684:1685 ~ "educ_hsless",
                               characteristic_id %in% 1687:1690 ~ "educ_college",
                               characteristic_id %in% 1691:1692 ~ "educ_university")) %>%
  na.omit() %>%
  group_by(district_code, education) %>%
  summarise(pop = sum(pop)) %>%
  mutate(pct = pop/sum(pop)) %>%
  dplyr::select(district_code, education, pct) %>%
  spread(education, pct)

sex_2016 <- demographics_2016 %>%
  filter(characteristic_id == 8) %>%
  mutate(sex_male = male_pop/(male_pop + female_pop),
         sex_female = female_pop/(male_pop + female_pop)) %>%
  dplyr::select(district_code, sex_male, sex_female)

race_2016 <- demographics_2016 %>%
  filter(characteristic_id %in% c(1324, 1337)) %>%
  mutate(minority = case_when(characteristic_id == 1324 ~ "minority",
                              characteristic_id == 1337 ~ "white")) %>%
  na.omit() %>%
  group_by(district_code, minority) %>%
  summarise(pop = sum(pop)) %>%
  mutate(pct = pop/sum(pop)) %>%
  dplyr::select(district_code, minority, pct) %>%
  spread(minority, pct)
  
demographics_2016 <- age_2016 %>%
  left_join(education_2016, by = "district_code") %>%
  left_join(sex_2016, by = "district_code") %>%
  left_join(race_2016, by = "district_code") %>%
  ungroup()

write_csv(demographics_2016, "Data/Processed/2016_demographics.csv")


#### Age, sex, education, and race for 2011 (2003 representation order) ####
long_form_2011 <- fread_to_tbl("Data/Raw/Demographics/district_long_form_2011.csv") %>%
  dplyr::select(-GNR) # contains race and education

names(long_form_2011) <- c("district_code", "province_name", "name_english", "topic", "characteristic", "footnote", "pop", 
                           "flag_total", "male_pop", "flag_male", "female_pop", "flag_female")

short_form_2011 <- fread_to_tbl("Data/Raw/Demographics/district_short_form_2011.csv") # contains age and sex

names(short_form_2011) <- c("district_code", "province_name", "name_english", "topic", "characteristic", "footnote", "pop", 
                            "flag_total", "male_pop", "flag_male", "female_pop", "flag_female")

demographics_2011_raw <- bind_rows(long_form_2011, short_form_2011) %>%
  filter(topic %in% c("Age characteristics", "Visible minority population", "Education")) %>%
  dplyr::select(district_code, topic, characteristic, pop, male_pop, female_pop)

age_2011 <- demographics_2011_raw %>%
  mutate(age = case_when(grepl("15 to 19|20 to 24|25 to 29", characteristic) ~ "age_1529",
                         grepl("30 to 34|35 to 39|40 to 44", characteristic) ~ "age_3044",
                         grepl("45 to 49|50 to 54|55 to 59|60 to 64", characteristic) ~ "age_4564",
                         grepl("65 to 69|70 to 74|75 to 79|80 to 84|85 years", characteristic) ~ "age_65")) %>%
  na.omit() %>%
  group_by(district_code, age) %>%
  summarise(pop = sum(pop)) %>%
  mutate(pct = pop/sum(pop)) %>%
  dplyr::select(district_code, age, pct) %>%
  spread(age, pct) %>%
  ungroup()

education_2011 <- demographics_2011_raw %>%
  mutate(education = case_when(grepl("No certificate|High school diploma", characteristic) ~ "educ_hsless",
                               grepl("Apprenticeship|CEGEP", characteristic) ~ "educ_college",
                               grepl("bachelor level", characteristic) ~ "educ_university")) %>%
  na.omit() %>%
  group_by(district_code, education) %>%
  summarise(pop = sum(pop)) %>%
  mutate(pct = pop/sum(pop)) %>%
  dplyr::select(district_code, education, pct) %>%
  spread(education, pct) %>%
  ungroup()

sex_2011 <- demographics_2011_raw %>%
  filter(characteristic == "Total population by age groups") %>%
  mutate(sex_male = male_pop/(male_pop + female_pop),
         sex_female = female_pop/(male_pop + female_pop)) %>%
  dplyr::select(district_code, sex_male, sex_female) %>%
  ungroup()

race_2011 <- demographics_2011_raw %>%
  mutate(minority = case_when(characteristic == "Total visible minority population" ~ "minority",
                              characteristic == "Not a visible minority" ~ "white")) %>%
  na.omit() %>%
  group_by(district_code, minority) %>%
  summarise(pop = sum(pop)) %>%
  mutate(pct = pop/sum(pop)) %>%
  dplyr::select(district_code, minority, pct) %>%
  spread(minority, pct) %>%
  ungroup()

demographics_2011 <- age_2011 %>%
  left_join(education_2011, by = "district_code") %>%
  left_join(sex_2011, by = "district_code") %>%
  left_join(race_2011, by = "district_code") %>%
  ungroup()

write_csv(demographics_2011, "Data/Processed/2011_demographics.csv")


#### Age, sex, education, and race for 2011 (2013 representation order) ####
long_form_2011_2013_order <- fread_to_tbl("Data/Raw/Demographics/district_2013_long_form_2011.csv") %>%
  dplyr::select(-GNR) # contains race and education

names(long_form_2011_2013_order) <- c("district_code", "province_name", "name_english", "topic", "characteristic", "footnote", "pop", 
                                      "flag_total", "male_pop", "flag_male", "female_pop", "flag_female")

short_form_2011_2013_order <- fread_to_tbl("Data/Raw/Demographics/district_2013_short_form_2011.csv") # contains age and sex

names(short_form_2011_2013_order) <- c("district_code", "province_name", "name_english", "topic", "characteristic", "footnote", "pop", 
                                       "flag_total", "male_pop", "flag_male", "female_pop", "flag_female")

demographics_2011_raw_2013_order <- bind_rows(long_form_2011, short_form_2011_2013_order) %>%
  filter(topic %in% c("Age characteristics", "Visible minority population", "Education")) %>%
  dplyr::select(district_code, topic, characteristic, pop, male_pop, female_pop)

age_2011_2013_order <- demographics_2011_raw_2013_order %>%
  mutate(age = case_when(grepl("15 to 19|20 to 24|25 to 29", characteristic) ~ "age_1529",
                         grepl("30 to 34|35 to 39|40 to 44", characteristic) ~ "age_3044",
                         grepl("45 to 49|50 to 54|55 to 59|60 to 64", characteristic) ~ "age_4564",
                         grepl("65 to 69|70 to 74|75 to 79|80 to 84|85 years", characteristic) ~ "age_65")) %>%
  na.omit() %>%
  group_by(district_code, age) %>%
  summarise(pop = sum(pop)) %>%
  mutate(pct = pop/sum(pop)) %>%
  dplyr::select(district_code, age, pct) %>%
  spread(age, pct) %>%
  ungroup()

education_2011_2013_order <- demographics_2011_raw_2013_order %>%
  mutate(education = case_when(grepl("No certificate|High school diploma", characteristic) ~ "educ_hsless",
                               grepl("Apprenticeship|CEGEP", characteristic) ~ "educ_college",
                               grepl("bachelor level", characteristic) ~ "educ_university")) %>%
  na.omit() %>%
  group_by(district_code, education) %>%
  summarise(pop = sum(pop)) %>%
  mutate(pct = pop/sum(pop)) %>%
  dplyr::select(district_code, education, pct) %>%
  spread(education, pct) %>%
  ungroup()

sex_2011_2013_order <- demographics_2011_raw_2013_order %>%
  filter(characteristic == "Total population by age groups") %>%
  mutate(sex_male = male_pop/(male_pop + female_pop),
         sex_female = female_pop/(male_pop + female_pop)) %>%
  dplyr::select(district_code, sex_male, sex_female) %>%
  ungroup()

race_2011_2013_order <- demographics_2011_raw_2013_order %>%
  mutate(minority = case_when(characteristic == "Total visible minority population" ~ "minority",
                              characteristic == "Not a visible minority" ~ "white")) %>%
  na.omit() %>%
  group_by(district_code, minority) %>%
  summarise(pop = sum(pop)) %>%
  mutate(pct = pop/sum(pop)) %>%
  dplyr::select(district_code, minority, pct) %>%
  spread(minority, pct) %>%
  ungroup()

demographics_2011_2013_order <- age_2011_2013_order %>%
  left_join(education_2011, by = "district_code") %>%
  left_join(sex_2011, by = "district_code") %>%
  left_join(race_2011, by = "district_code") %>%
  ungroup()

write_csv(demographics_2011_2013_order, "Data/Processed/2011_demographics_2013_order.csv")