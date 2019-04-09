#### NATIONAL ####
source("Code/poll_scrape_clean.R")

## Add weights
national_polls <- national_polls %>%
  mutate(numeric_date = as.numeric(date), 
         weight = (age <= 60)*exp(-(age)^(1/3))/sqrt(MOE/100)/(ifelse(IVR, 3, 1)),
         loess_weight = 1/sqrt(MOE/100)/(ifelse(IVR, 3, 1))) %>%
  filter(!(pollster == "Abacus Data" & date %in% as.Date(paste0("2019-03-0", c(7, 6, 4, 3)))))

## Weighted average
national_polls %>% 
  melt(id.vars = c("pollster", "date", "age", "MOE", "n", "mode", "IVR", "weight")) %>%
  filter(variable %in% c("LPC", "CPC", "NDP", "BQ", "GPC", "PPC")) %>%
  group_by(party = variable) %>%
  summarise(average = Hmisc::wtd.mean(value, weights = weight, na.rm = TRUE),
            sd = sqrt(Hmisc::wtd.var(value, weights = weight, na.rm = TRUE)))

## Weighted covariance (except People's Party)
national_polls_matrix <- national_polls %>%
  dplyr::select(weight, LPC, CPC, NDP, BQ, GPC, PPC) %>%
  na.omit()
  
national_polls_covariance <- cov.wt(national_polls_matrix %>% dplyr::select(-weight), national_polls_matrix$weight)$cov

## Fit loess models
national_loess.LPC <- loess(LPC~numeric_date, data = national_polls, weights = loess_weight)
national_loess.CPC <- loess(CPC~numeric_date, data = national_polls, weights = loess_weight)
national_loess.NDP <- loess(NDP~numeric_date, data = national_polls, weights = loess_weight)
national_loess.GPC <- loess(GPC~numeric_date, data = national_polls, weights = loess_weight)
national_loess.PPC <- loess(PPC~numeric_date, data = national_polls, weights = loess_weight)

## House effect estimation
house_effects <- national_polls %>%
  mutate(numeric_date = as.numeric(date),
         LPC_pred = predict(national_loess.LPC, newdata = .),
         CPC_pred = predict(national_loess.CPC, newdata = .),
         NDP_pred = predict(national_loess.NDP, newdata = .),
         GPC_pred = predict(national_loess.GPC, newdata = .),
         PPC_pred = predict(national_loess.PPC, newdata = .)) %>%
  group_by(pollster) %>%
  summarise(LPC_house = wtd.mean(LPC - LPC_pred, weights = loess_weight, na.rm = TRUE),
            CPC_house = wtd.mean(CPC - CPC_pred, weights = loess_weight, na.rm = TRUE),
            NDP_house = wtd.mean(NDP - NDP_pred, weights = loess_weight, na.rm = TRUE),
            GPC_house = wtd.mean(GPC - GPC_pred, weights = loess_weight, na.rm = TRUE),
            PPC_house = wtd.mean(PPC - PPC_pred, weights = loess_weight, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(full_house = (3*abs(LPC_house) + 3*abs(CPC_house) + 2*abs(NDP_house) + abs(GPC_house))/9 + 1)

## Plot national polls
ggplot(national_polls %>%
         melt(measure.vars = c("LPC", "CPC", "NDP", "GPC", "PPC"),
              variable.name = "Party", value.name = "Poll"), 
       aes(x = date, y = Poll, col = Party)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "loess", span = 0.2, size = 1) +
  scale_colour_manual(name = "Party", values = national_colors, labels = national_parties) +
  lims(x = c(as.Date("2018-01-01"), today())) +
  labs(title = "2019 Canadian federal election polling",
       subtitle = "National", x = "Date", y = "%")

#### PROVINCIAL ####
provincial_polls <- read_csv("Data/provincial_polling.csv") %>%
  reshape(varying = grep("\\.", names(read_csv("Data/provincial_polling.csv")), value = TRUE), 
          idvar = c("pollster", "last_date", "mode"), direction = "long") %>%
  left_join(read_csv("Data/poll_spreads.csv"), by = "pollster", all.x = TRUE) %>%
  
  # Calculate poll age
  mutate(last_date = as.Date(last_date, format = "%d-%b-%y"),
         date = last_date - floor(spread/2),
         age = as.numeric(lubridate::today() - date)) %>%
  
  arrange(age) %>%
  dplyr::select(pollster, date, age, mode, province = time, LPC, CPC, NDP, BQ, GPC, PPC) %>%
  mutate(province = case_when(province == "BC" ~ "British Columbia",
                              province != "BC" ~ province)) %>%
  as.tbl() %>%
  mutate(weight = (age <= 60)*exp(-age^(1/3))/ifelse(mode == "IVR", 3, 1))

## Plotting
provincial_polls %>%
  melt(id.vars = c("pollster", "date", "age", "mode", "province"), variable.name = "Party", value.name = "Poll") %>%
  mutate(Poll = as.numeric(Poll)) %>%
  ggplot(aes(x = date, y = Poll, col = Party)) +
  facet_wrap(~province) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "loess", span = 0.5, size = 1) +
  geom_vline(xintercept = as.Date("2019-02-07")) +
  scale_colour_manual(name = "Party", values = quebec_colors, labels = quebec_parties) +
  labs(title = "2019 Canadian federal election polling",
       subtitle = "By province", x = "Date", y = "%") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, size = 7))

## Regional covariances
ontario_polls <- provincial_polls %>%
  filter(province == "Ontario") %>%
  dplyr::select(weight, LPC, CPC, NDP, GPC, PPC) %>%
  na.omit()

atlantic_polls <- provincial_polls %>%
  filter(province == "Atlantic") %>%
  dplyr::select(weight, LPC, CPC, NDP, GPC, PPC) %>%
  na.omit()

quebec_polls <- provincial_polls %>%
  filter(province == "Quebec") %>%
  dplyr::select(weight, LPC, CPC, NDP, BQ, GPC, PPC) %>%
  na.omit()

prairie_polls <- provincial_polls %>%
  filter(province == "Prairie") %>%
  dplyr::select(weight, LPC, CPC, NDP, GPC, PPC) %>%
  na.omit()

alberta_polls <- provincial_polls %>%
  filter(province == "Alberta") %>%
  dplyr::select(weight, LPC, CPC, NDP, GPC, PPC) %>%
  na.omit()

bc_polls <- provincial_polls %>%
  filter(province == "British Columbia") %>%
  dplyr::select(weight, LPC, CPC, NDP, GPC, PPC) %>%
  na.omit()

ontario_poll_covariance <- cov.wt(ontario_polls %>% dplyr::select(-weight), wt = ontario_polls$weight)$cov
atlantic_poll_covariance <- cov.wt(atlantic_polls %>% dplyr::select(-weight), wt = atlantic_polls$weight)$cov
quebec_poll_covariance <- cov.wt(quebec_polls %>% dplyr::select(-weight), wt = quebec_polls$weight)$cov
prairie_poll_covariance <- cov.wt(prairie_polls %>% dplyr::select(-weight), wt = prairie_polls$weight)$cov
alberta_poll_covariance <- cov.wt(alberta_polls %>% dplyr::select(-weight), wt = alberta_polls$weight)$cov
bc_poll_covariance <- cov.wt(bc_polls %>% dplyr::select(-weight), wt = bc_polls$weight)$cov