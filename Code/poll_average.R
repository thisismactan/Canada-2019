#### NATIONAL ####
source("Code/poll_scrape_clean.R")

## Add weights
national_polls <- national_polls %>%
  mutate(numeric_date = as.numeric(date), 
         weight = (age < 90)*exp(-(age)^(1/3))/sqrt(MOE/100)/(ifelse(IVR, 3, 1)),
         loess_weight = 1/sqrt(MOE/100)/(ifelse(IVR, 3, 1)))

## Weighted average
national_polls %>% 
  melt(id.vars = c("pollster", "date", "age", "MOE", "n", "mode", "IVR", "weight")) %>%
  filter(variable %in% c("LPC", "CPC", "NDP", "BQ", "GPC", "PPC")) %>%
  group_by(party = variable) %>%
  summarise(average = Hmisc::wtd.mean(value, weights = weight, na.rm = TRUE),
            sd = sqrt(Hmisc::wtd.var(value, weights = weight, na.rm = TRUE)))

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
  geom_smooth(method = "loess", span = 0.3, size = 1) +
  scale_colour_manual(name = "Party", values = national_colors, labels = national_parties) +
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
  as.tbl()

## Plotting
provincial_polls %>%
  melt(id.vars = c("pollster", "date", "age", "mode", "province"), variable.name = "Party", value.name = "Poll") %>%
  mutate(Poll = as.numeric(Poll)) %>%
  ggplot(aes(x = date, y = Poll, col = Party)) +
  facet_wrap(~province) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "loess", span = 2/3, size = 1) +
  scale_colour_manual(name = "Party", values = quebec_colors, labels = quebec_parties) +
  labs(title = "2019 Canadian federal election polling",
       subtitle = "By province", x = "Date", y = "%") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, size = 7))
