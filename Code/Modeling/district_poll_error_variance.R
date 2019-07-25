source("Code/library.R")

# Calculate poll average throughout 2015 campaign

## Read in national polls
national_polls_2015 <- read_csv("Data/Processed/national_polls_2015.csv")
quebec_polls_2015 <- read_csv("Data/Processed/quebec_polls_2015.csv")

dates_2015 <- seq(as.Date("2015-08-01"), as.Date("2015-10-19"), by = 1)
avg_list.2015 <- vector("list", length(dates))

for(i in 1:length(dates_2015)) {
  poll_data_2015_timeline <- national_polls_2015 %>%
    filter(median_date <= dates_2015[i]) %>%
    mutate(age = as.numeric(dates_2015[i] - median_date),
           weight = 3*(age <= 45)*exp(-(age)^(0.25))/sqrt(moe)/ifelse(ivr == 1, 3, 1))
  
  avg_list.2015[[i]] <- poll_data_2015_timeline %>%
    summarise(LPC = wtd.mean(LPC, weight),
              CPC = wtd.mean(CPC, weight),
              NDP = wtd.mean(NDP, weight),
              Green = wtd.mean(GPC, weight)) %>%
    mutate(median_date = dates_2015[i]) %>%
    melt(id.vars = "median_date", variable.name = "party", value.name = "natl_pct")
}

poll_averages_2015 <- bind_rows(avg_list.2015)

# Read in 2015 district-level polls and merge in results, national poll averages
historical_district_polls <- read_csv("Data/Processed/historical_district_polls.csv") %>%
  left_join(poll_averages_2015, by = c("median_date", "party")) %>%
  mutate(lean = pct - natl_pct) %>%
  left_join(poll_averages_2015 %>% filter(median_date == as.Date("2015-10-19")), by = "party") %>%
  left_join(results_2015_long %>% filter(variable == "pct") %>% dplyr::select(district_code, party, value), by = c("district_code", "party")) %>%
  mutate(age = as.numeric(election_date - median_date.x), 
         weight = 10*(n^0.25)/exp((age/7)^0.3),
         pct_adj = lean + natl_pct.y,
         pct_actual = 100*as.numeric(value)) %>%
  dplyr::select(name_english, district_code, pollster, date = median_date.x, n, weight, party, pct_adj, pct_actual) %>%
  mutate(error = (pct_adj - pct_actual)/100)

# Compute error variance
district_poll_error_variance <- historical_district_polls %>%
  summarise(mse = wtd.mean(error^2, weight)) %>%
  pull(mse)
