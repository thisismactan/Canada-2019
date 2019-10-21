#### NATIONAL ####
source("Code/poll_scrape_clean.R")

## Add weights
national_polls <- national_polls %>%
  mutate(numeric_date = as.numeric(date), 
         loess_weight = 1/sqrt(MOE/100)/(ifelse(IVR, 3, 1) * ifelse(rolling, 3, 1))) %>%
  filter(!(pollster == "Abacus Data" & date %in% as.Date(paste0("2019-03-0", c(7, 6, 4, 3)))))

## Fit loess models
national_loess.LPC <- loess(LPC~numeric_date, data = national_polls, weights = loess_weight)
national_loess.CPC <- loess(CPC~numeric_date, data = national_polls, weights = loess_weight)
national_loess.NDP <- loess(NDP~numeric_date, data = national_polls, weights = loess_weight)
national_loess.BQ <- loess(BQ~numeric_date, data = national_polls, weights = loess_weight)
national_loess.GPC <- loess(GPC~numeric_date, data = national_polls, weights = loess_weight)
national_loess.PPC <- loess(PPC~numeric_date, data = national_polls, weights = loess_weight)

## House effect estimation
house_effects <- national_polls %>%
  mutate(numeric_date = as.numeric(date),
         LPC_pred = predict(national_loess.LPC, newdata = .),
         CPC_pred = predict(national_loess.CPC, newdata = .),
         NDP_pred = predict(national_loess.NDP, newdata = .),
         BQ_pred = predict(national_loess.BQ, newdata = .),
         GPC_pred = predict(national_loess.GPC, newdata = .),
         PPC_pred = predict(national_loess.PPC, newdata = .)) %>%
  group_by(pollster) %>%
  summarise(LPC_house = wtd.mean(LPC - LPC_pred, weights = loess_weight, na.rm = TRUE),
            CPC_house = wtd.mean(CPC - CPC_pred, weights = loess_weight, na.rm = TRUE),
            NDP_house = wtd.mean(NDP - NDP_pred, weights = loess_weight, na.rm = TRUE),
            BQ_house = wtd.mean(BQ - BQ_pred, weights = loess_weight, na.rm = TRUE),
            GPC_house = wtd.mean(GPC - GPC_pred, weights = loess_weight, na.rm = TRUE),
            PPC_house = wtd.mean(PPC - PPC_pred, weights = loess_weight, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(full_house = (3*abs(LPC_house) + 3*abs(CPC_house) + 2*abs(NDP_house) + abs(GPC_house))/9 + 1)

national_polls.adjusted <- national_polls %>%
  left_join(house_effects, by = "pollster") %>%
  mutate(weight = 3*(age <= 40)*exp(-(age)^(0.8))/sqrt(MOE/100)/(ifelse(IVR, 3, 1))/sqrt(full_house),
         LPC = LPC - LPC_house/2,
         CPC = CPC - CPC_house/2,
         BQ = BQ - BQ_house/2,
         NDP = NDP - NDP_house/2,
         GPC = GPC - GPC_house/2)

write_rds(national_polls %>%
            reshape2::melt(measure.vars = c("LPC", "CPC", "NDP", "GPC", "PPC"),
                           variable.name = "Party", value.name = "Poll") %>%
            mutate(Party = case_when(Party == "LPC" ~ "Liberal",
                                     Party == "CPC" ~ "Conservative",
                                     Party == "NDP" ~ "NDP",
                                     Party == "GPC" ~ "Green",
                                     Party == "PPC" ~ "People's"),
                   Party = ordered(Party, levels = c("Liberal", "Conservative", "NDP", "Green", "People's"))) %>%
            arrange(Party), "Shiny-app/national_polls.rds")

write_rds(national_polls.adjusted %>%
            reshape2::melt(measure.vars = c("LPC", "CPC", "NDP", "GPC", "PPC"),
                           variable.name = "Party", value.name = "Poll") %>%
            mutate(Party = case_when(Party == "LPC" ~ "Liberal",
                                     Party == "CPC" ~ "Conservative",
                                     Party == "NDP" ~ "NDP",
                                     Party == "GPC" ~ "Green",
                                     Party == "PPC" ~ "People's"),
                   Party = ordered(Party, levels = c("Liberal", "Conservative", "NDP", "Green", "People's"))) %>%
            arrange(Party), "Shiny-app/national_polls_adjusted.rds")

## Weighted average
national_avg <- national_polls.adjusted %>% 
  melt(id.vars = c("pollster", "date", "age", "MOE", "n", "mode", "IVR", "weight")) %>%
  filter(variable %in% c("LPC", "CPC", "NDP", "BQ", "GPC", "PPC")) %>%
  group_by(party = variable) %>%
  summarise(average = Hmisc::wtd.mean(value, weights = weight, na.rm = TRUE),
            sd = sqrt(Hmisc::wtd.var(value, weights = weight, na.rm = TRUE)))

national_avg %>%
  mutate(lower = average - 1.644*sd,
         upper = average + 1.644*sd) %>%
  ggplot(aes(x = party, y = average, fill = party)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower, ymax = upper), col = "#666666") +
  geom_text(aes(label = round(average, 1), y = average + 1)) +
  scale_fill_manual(name = "Party", values = c("red", "blue", "darkorange1", "#8ECEF9", "green4", "midnightblue"),
                    labels = c("Liberal", "Conservative", "NDP", "Bloc Québécois", "Green", "People's Party")) +
  theme(axis.ticks.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "2019 Canadian federal election national polling", y = "%", caption = "Error bars indicate 90% CI",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

## Weighted covariance (except People's Party)
                   
national_polls_matrix <- national_polls.adjusted %>%
  dplyr::select(weight, LPC, CPC, NDP, BQ, GPC, PPC) %>%
  na.omit()
  
national_polls_covariance <- cov.wt(national_polls_matrix %>% dplyr::select(-weight), national_polls_matrix$weight)$cov / 
  (sum(national_polls_matrix$weight)^2 / sum(national_polls_matrix$weight^2))

## Plot national polls
ggplot(national_polls.adjusted %>%
         melt(measure.vars = c("LPC", "CPC", "NDP", "GPC", "PPC"),
              variable.name = "Party", value.name = "Poll"), 
       aes(x = date, y = Poll, col = Party)) +
  geom_vline(xintercept = as.Date("2019-10-21")) +
  geom_point(aes(size = loess_weight), alpha = 0.4) +
  geom_smooth(aes(weight = loess_weight), method = "loess", span = 0.25, size = 1) +
  scale_colour_manual(name = "Party", values = national_colors_abbr) +
  scale_size_continuous(name = "Weight", range = c(0.1, 3)) +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y", limits = as.Date(c("2018-01-01", "2019-10-21"))) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  scale_y_continuous(breaks = 10*(0:6)) +
  labs(title = "2019 Canadian federal election polling",
       subtitle = "National", x = "Date", y = "%")

#### PROVINCIAL ####
provincial_polls <- read_csv("Data/provincial_polling.csv") %>%
  reshape(varying = grep("\\.", names(read_csv("Data/provincial_polling.csv")), value = TRUE), 
          idvar = c("pollster", "median_date", "mode", "LV", "n"), direction = "long") %>%
  left_join(read_csv("Data/poll_spreads.csv"), by = "pollster", all.x = TRUE) %>%
  
  # Calculate poll age
  mutate(date = as.Date(median_date, format = "%d-%b-%y"),
         age = as.numeric(lubridate::today() - date)) %>%
  
  arrange(age) %>%
  dplyr::select(pollster, date, age, mode, n, LV, province = time, LPC, CPC, NDP, BQ, GPC, PPC) %>%
  mutate(province = case_when(province == "BC" ~ "British Columbia",
                              province != "BC" ~ province)) %>%
  as.tbl() %>%
  left_join(house_effects, by = "pollster") %>%
  mutate(weight = 100*(age <= 40)*(ifelse(LV, 3, 1))*exp(-age^(2/3))/ifelse(mode == "IVR", 3, 1)/sqrt(sqrt(n))/full_house,
         loess_weight = 100*(ifelse(LV, 3, 1))/ifelse(mode == "IVR", 3, 1)/sqrt(sqrt(n))/full_house)

provincial_polls_adjusted <- provincial_polls %>%
  mutate(LPC = LPC - 0.5*LPC_house,
         CPC = CPC - 0.5*CPC_house,
         NDP = NDP - 0.5*NDP_house,
         BQ = BQ - 2.055*BQ_house,
         GPC = GPC - 0.5*GPC_house)

## Plotting
provincial_polls_adjusted %>%
  melt(id.vars = c("pollster", "date", "age", "n", "mode", "LV", "province", "weight", "loess_weight", "LPC_house", "CPC_house", "NDP_house",
                   "GPC_house", "BQ_house", "PPC_house", "full_house"), variable.name = "Party", value.name = "Poll") %>%
  mutate(Poll = as.numeric(Poll),
         Party = case_when(Party == "LPC" ~ "Liberal",
                           Party == "CPC" ~ "Conservative",
                           Party == "NDP" ~ "NDP",
                           Party == "BQ" ~ "Bloc",
                           Party == "GPC" ~ "Green",
                           Party == "PPC" ~ "People's") %>%
           ordered(levels = c("Liberal", "Conservative", "NDP", "Green", "Bloc", "People's"))) %>%
  ggplot(aes(x = date, y = Poll, col = Party)) +
  facet_wrap(~province) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(aes(weight = loess_weight), method = "loess", span = 1/3, size = 1) +
  scale_colour_manual(name = "Party", values = quebec_colors) +
  labs(title = "2019 Canadian federal election polling",
       subtitle = "By province", x = "Date", y = "%") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 0.5))

## Regional covariances
ontario_polls <- provincial_polls_adjusted %>%
  filter(province == "Ontario") %>%
  dplyr::select(weight, LPC, CPC, NDP, GPC, PPC) %>%
  na.omit()

atlantic_polls <- provincial_polls_adjusted %>%
  filter(province == "Atlantic") %>%
  dplyr::select(weight, LPC, CPC, NDP, GPC, PPC) %>%
  na.omit()

quebec_polls <- provincial_polls_adjusted %>%
  filter(province == "Quebec") %>%
  dplyr::select(weight, LPC, CPC, NDP, BQ, GPC, PPC) %>%
  na.omit()

prairie_polls <- provincial_polls_adjusted %>%
  filter(province == "Prairie") %>%
  dplyr::select(weight, LPC, CPC, NDP, GPC, PPC) %>%
  na.omit()

alberta_polls <- provincial_polls_adjusted %>%
  filter(province == "Alberta") %>%
  dplyr::select(weight, LPC, CPC, NDP, GPC, PPC) %>%
  na.omit()

bc_polls <- provincial_polls_adjusted %>%
  filter(province == "British Columbia") %>%
  dplyr::select(weight, LPC, CPC, NDP, GPC, PPC) %>%
  na.omit()

## Poll covariances
ontario_polls_covariance <- cov.wt(ontario_polls %>% dplyr::select(-weight), wt = ontario_polls$weight)$cov 
atlantic_polls_covariance <- cov.wt(atlantic_polls %>% dplyr::select(-weight), wt = atlantic_polls$weight)$cov
quebec_polls_covariance <- cov.wt(quebec_polls %>% dplyr::select(-weight), wt = quebec_polls$weight)$cov
prairie_polls_covariance <- cov.wt(prairie_polls %>% dplyr::select(-weight), wt = prairie_polls$weight)$cov
alberta_polls_covariance <- cov.wt(alberta_polls %>% dplyr::select(-weight), wt = alberta_polls$weight)$cov
bc_polls_covariance <- cov.wt(bc_polls %>% dplyr::select(-weight), wt = bc_polls$weight)$cov 

## Regional polling averages
regional_polls <- bind_rows(
  alberta_polls %>% mutate(region = "Alberta"),
  atlantic_polls %>% mutate(region = "Atlantic Canada"),
  bc_polls %>% mutate(region = "British Columbia"),
  ontario_polls %>% mutate(region = "Ontario"),
  prairie_polls %>% mutate(region = "Prairie"),
  quebec_polls %>% mutate(region = "Quebec")
) %>%
  dplyr::select(region, weight, LPC, CPC, NDP, BQ, GPC, PPC)

regional_polls %>%
  filter(weight > 0) %>%
  melt(id.vars = c("region", "weight"), variable.name = "party", value.name = "pct") %>%
  group_by(region, party) %>%
  summarise(avg = wtd.mean(pct, weight),
            sd = sqrt(wtd.var(pct, weight))*n()/(n() - 1),
            n_eff = sum(weight)^2 / sum(weight^2)) %>%
  mutate(upper = pmin(avg + 1.645*sd / sqrt(n_eff), 100),
         lower = pmax(avg - 1.645*sd / sqrt(n_eff), 0)) %>%
  ggplot(aes(x = party, y = avg, fill = party)) +
  facet_wrap(~region) +
  geom_col() +
  geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 2/3) +
  geom_text(aes(y = avg + 2.5, label = round(avg, 1)), size = 3) +
  scale_fill_manual(name = "Party", values = quebec_colors_abbr) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()) +
  labs(title = "2019 Canadian federal election polling by region", caption = "Error bars indicate 90% CIs", y = "%",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))
