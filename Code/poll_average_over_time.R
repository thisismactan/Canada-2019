## Creates a tibble containing the poll average for each day
source("Code/poll_average.R")

## Unadjusted average
dates <- seq(as.Date("2016-01-01"), today(), by = 1)
avg_adjusted_list <- avg_unadjusted_list <- sd_adjusted_list <- sd_unadjusted_list <- vector("list", length(dates))

start_time <- Sys.time()
for(i in 1:length(dates)) {
  poll_data_unadjusted_timeline <- national_polls %>%
    filter(date <= dates[i]) %>%
    left_join(house_effects, by = "pollster") %>%
    mutate(age = as.numeric(dates[i] - date),
           weight = 3*(age <= 60)*exp(-(age)^(0.5))/sqrt(MOE/100)/(ifelse(IVR, 3, 1)))
  
  poll_data_adjusted_timeline <- national_polls.adjusted %>%
    filter(date <= dates[i]) %>%
    mutate(age = as.numeric(dates[i] - date),
           weight = 3*(age <= 60)*exp(-(age)^(0.5))/sqrt(MOE/100)/(ifelse(IVR, 3, 1)*sqrt(full_house))
    )
  
  avg_unadjusted_list[[i]] <- poll_data_unadjusted_timeline %>%
    summarise(Liberal = wtd.mean(LPC, weight),
              Conservative = wtd.mean(CPC, weight),
              NDP = wtd.mean(NDP, weight),
              Green = wtd.mean(GPC, weight),
              `People's` = wtd.mean(PPC, weight)) %>%
    mutate(date = dates[i]) %>%
    melt(id.vars = "date", variable.name = "Party", value.name = "pct")
  
  avg_adjusted_list[[i]] <- poll_data_adjusted_timeline %>%
    summarise(Liberal = wtd.mean(LPC, weight),
              Conservative = wtd.mean(CPC, weight),
              NDP = wtd.mean(NDP, weight),
              Green = wtd.mean(GPC, weight),
              `People's` = wtd.mean(PPC, weight)) %>%
    mutate(date = dates[i]) %>%
    melt(id.vars = "date", variable.name = "Party", value.name = "pct")
  
  sd_unadjusted_list[[i]] <- poll_data_unadjusted_timeline %>%
    summarise(Liberal = sqrt(wtd.var(LPC, weight)),
              Conservative = sqrt(wtd.var(CPC, weight)),
              NDP = sqrt(wtd.var(NDP, weight)),
              Green = sqrt(wtd.var(GPC, weight)),
              `People's` = sqrt(wtd.var(PPC, weight))) %>%
    mutate(date = dates[i]) %>%
    melt(id.vars = "date", variable.name = "Party", value.name = "sd")
  
  sd_adjusted_list[[i]] <- poll_data_adjusted_timeline %>%
    summarise(Liberal = sqrt(wtd.var(LPC, weight)),
              Conservative = sqrt(wtd.var(CPC, weight)),
              NDP = sqrt(wtd.var(NDP, weight)),
              Green = sqrt(wtd.var(GPC, weight)),
              `People's` = sqrt(wtd.var(PPC, weight))) %>%
    mutate(date = dates[i]) %>%
    melt(id.vars = "date", variable.name = "Party", value.name = "sd")
}

poll_averages_unadjusted <- bind_rows(avg_unadjusted_list) %>%
  arrange(Party, date) %>%
  group_by(Party) %>%
  mutate(pct = (lag(pct, 2) + lag(pct, 1) + pct + lead(pct, 1) + lead(pct, 2)) / 5)

poll_averages_adjusted <- bind_rows(avg_adjusted_list) %>%
  arrange(Party, date) %>%
  group_by(Party) %>%
  mutate(pct = (lag(pct, 2) + lag(pct, 1) + pct + lead(pct, 1) + lead(pct, 2)) / 5)

poll_sds_unadjusted <- bind_rows(sd_unadjusted_list) %>%
  arrange(Party, date) %>%
  group_by(Party) %>%
  mutate(sd = (lag(sd, 2) + lag(sd, 1) + sd + lead(sd, 1) + lead(sd, 2)) / 5)

poll_sds_adjusted <- bind_rows(sd_adjusted_list) %>%
  arrange(Party, date) %>%
  group_by(Party) %>%
  mutate(sd = (lag(sd, 2) + lag(sd, 1) + sd + lead(sd, 1) + lead(sd, 2)) / 5)

write_csv(poll_averages_unadjusted, "Shiny-app/poll_averages_unadjusted.csv")
write_csv(poll_averages_adjusted, "Shiny-app/poll_averages_adjusted.csv")
write_csv(poll_sds_unadjusted, "Shiny-app/poll_sds_unadjusted.csv")
write_csv(poll_sds_adjusted, "Shiny-app/poll_sds_adjusted.csv")
