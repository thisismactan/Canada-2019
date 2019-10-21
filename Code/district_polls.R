source("Code/Modeling/district_poll_error_variance.R")

# Read in current district polls
district_polls <- read_csv("Data/district_polling.csv") %>%
  left_join(national_avg, by = "party") %>%
  mutate(average = case_when(floor(district_code / 1000) == 24 & party == "LPC" ~ quebec_means[1],
                             floor(district_code / 1000) == 24 & party == "CPC" ~ quebec_means[2],
                             floor(district_code / 1000) == 24 & party == "NDP" ~ quebec_means[3],
                             floor(district_code / 1000) == 24 & party == "BQ" ~ quebec_means[4],
                             floor(district_code / 1000) == 24 & party == "GPC" ~ quebec_means[5],
                             floor(district_code / 1000) == 24 & party == "PPC" ~ quebec_means[6],
                             floor(district_code / 1000) != 24 ~ average),
         pct_adj = case_when(!is.na(lean) ~ lean + average,
                             is.na(lean) ~ pct)) %>%
  mutate(age = as.numeric(today() - median_date),
         weight = 10*(n^0.25)/exp((age/7)^0.3))

district_poll_avg <- district_polls %>%
  group_by(district_code, party) %>%
  summarise(pct = wtd.mean(pct_adj, weight)/100,
            total_n = sum(n),
            most_recent = max(median_date)) %>%
  mutate(age_weeks = pmin(as.numeric(today() - most_recent)/7, 4),
         variance = exp(0.1*age_weeks)*0.25/total_n + district_poll_error_variance/2) %>%
  dplyr::select(district_code, variance, age_weeks, party, pct) %>%
  spread(party, pct) %>%
  ungroup() %>%
  right_join(district_key_2013, by = "district_code") %>%
  dplyr::select(-name_english, -population)

names(district_poll_avg) <- c("district_code", "variance", "age_weeks", "BQ_poll", "CPC_poll", "GPC_poll", "Ind_poll", "LPC_poll", "NDP_poll", "PPC_poll")
