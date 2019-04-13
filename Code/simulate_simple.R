## Run necessaries
source("Code/poll_average.R")
source("Code/Modeling/poll_error_variance.R")
source("Code/Modeling/model_error_variance.R")
source("Code/shape_2019_data.R")

## Simulate draws from national polling
means <- national_polls.adjusted %>% 
  melt(id.vars = c("pollster", "date", "age", "MOE", "n", "mode", "IVR", "weight")) %>%
  filter(variable %in% c("LPC", "CPC", "NDP", "BQ", "GPC", "PPC")) %>%
  group_by(party = variable) %>%
  summarise(average = Hmisc::wtd.mean(value, weights = weight, na.rm = TRUE)) %>%
  pull(average)

national_poll_sims <- mvrnorm(10000, means, national_polls_covariance)

## 
LPC_preds <- predict(model_LPC.linear, newdata = data_2019.simple)
CPC_preds <- predict(model_CPC.linear, newdata = data_2019.simple)
NDP_preds <- predict(model_NDP.linear, newdata = data_2019.simple)
Bloc_preds <- predict(model_Bloc.linear, newdata = data_2019.simple)
Green_preds <- predict(model_Green.linear, newdata = data_2019.simple)

predictions <- tibble(district_code = data_2019.simple$district_code,
                      name_english = data_2019.simple$name_english,
                      region = data_2019.simple$region,
                      population = data_2019.simple$population,
                      LPC_cand = data_2019.simple$LPC_cand,
                      CPC_cand = data_2019.simple$CPC_cand,
                      NDP_cand = data_2019.simple$NDP_cand,
                      Bloc_cand = data_2019.simple$Bloc_cand,
                      Green_cand = data_2019.simple$Green_cand,
                      LPC_lag = data_2019.simple$LPC_lag,
                      CPC_lag = data_2019.simple$CPC_lag,
                      NDP_lag = data_2019.simple$NDP_lag,
                      Bloc_lag = data_2019.simple$Bloc_lag,
                      Green_lag = data_2019.simple$Green_lag,
                      LPC_vote = LPC_preds,
                      CPC_vote = CPC_preds,
                      NDP_vote = NDP_preds,
                      Bloc_vote = Bloc_preds*(district_code >= 24000 & district_code <= 24999),
                      Green_vote = Green_preds) %>%
  mutate(total_vote = LPC_vote + CPC_vote + NDP_vote + Bloc_vote + Green_vote) %>%
  mutate(winner = case_when(LPC_vote == pmax(LPC_vote, CPC_vote, NDP_vote, Bloc_vote, Green_vote) ~ "Liberal",
                            CPC_vote == pmax(LPC_vote, CPC_vote, NDP_vote, Bloc_vote, Green_vote) ~ "Conservative",
                            NDP_vote == pmax(LPC_vote, CPC_vote, NDP_vote, Bloc_vote, Green_vote) ~ "NDP",
                            Bloc_vote == pmax(LPC_vote, CPC_vote, NDP_vote, Bloc_vote, Green_vote) ~ "Bloc",
                            Green_vote == pmax(LPC_vote, CPC_vote, NDP_vote, Bloc_vote, Green_vote) ~ "Green"),
         winner_lag = case_when(LPC_lag == pmax(LPC_lag, CPC_lag, NDP_lag, Bloc_lag, Green_lag) ~ "Liberal",
                                CPC_lag == pmax(LPC_lag, CPC_lag, NDP_lag, Bloc_lag, Green_lag) ~ "Conservative",
                                NDP_lag == pmax(LPC_lag, CPC_lag, NDP_lag, Bloc_lag, Green_lag) ~ "NDP",
                                Bloc_lag == pmax(LPC_lag, CPC_lag, NDP_lag, Bloc_lag, Green_lag) ~ "Bloc",
                                Green_lag == pmax(LPC_lag, CPC_lag, NDP_lag, Bloc_lag, Green_lag) ~ "Green"))

predictions %>%
  group_by(winner) %>%
  summarise(seats = n())

predictions %>%
  group_by(winner, region) %>%
  summarise(seats = n()) %>%
  spread(winner, seats)

predictions %>%
  group_by(winner, winner_lag) %>%
  summarise(seats = n()) %>%
  spread(winner, seats)

predictions %>%
  mutate(LPC_swing = LPC_vote - LPC_lag,
         CPC_swing = CPC_vote - CPC_lag,
         NDP_swing = NDP_vote - NDP_lag,
         Bloc_swing = Bloc_vote - Bloc_lag,
         Green_swing = Green_vote - Green_lag) %>%
  dplyr::select(district_code, LPC_swing, CPC_swing, NDP_swing, Bloc_swing, Green_swing) %>%
  melt(id.vars = "district_code", variable.name = "party", value.name = "pp") %>%
  ggplot(aes(x = pp, fill = party)) +
  facet_wrap(~party, scales = "free_x") +
  geom_histogram(col = "black") +
  scale_fill_manual(name = "Party", values = c("red", "blue", "darkorange1", "#8ECEF9", "green4"),
                    labels = c("Liberal", "Conservative", "NDP", "Bloc Québécois", "Green"))

predictions %>%
  summarise(LPC_votes = sum(LPC_vote*population),
            CPC_votes = sum(CPC_vote*population),
            NDP_votes = sum(NDP_vote*population),
            Bloc_votes = sum(Bloc_vote*population),
            Green_votes = sum(Green_vote*population)) %>%
  mutate(LPC_pct = LPC_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         CPC_pct = CPC_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         NDP_pct = NDP_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         Bloc_pct = Bloc_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         Green_pct = Green_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes)) %>%
  dplyr::select(ends_with("pct"))

## Total covariance matrices (one for each province)