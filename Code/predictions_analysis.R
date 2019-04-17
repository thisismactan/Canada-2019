## Predictions
LPC_preds <- predict(model_LPC.linear, newdata = data_2019.simple)
CPC_preds <- predict(model_CPC.linear, newdata = data_2019.simple)
NDP_preds <- predict(model_NDP.linear, newdata = data_2019.simple)
Bloc_preds <- predict(model_Bloc.linear, newdata = data_2019.simple)
Green_preds <- predict(model_Green.linear, newdata = data_2019.simple)

predictions <- tibble(district_code = data_2019.simple$district_code,
                      name_english = data_2019.simple$name_english,
                      region = data_2019.simple$region,
                      population = data_2019.simple$population,
                      total_votes_2015 = results_2015$total_votes,
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
                            Green_vote == pmax(LPC_vote, CPC_vote, NDP_vote, Bloc_vote, Green_vote) ~ "Green")
         ) %>%
  left_join(read_csv("Data/incumbents.csv") %>% dplyr::select(-name_english), by = "district_code")

## Distribution of vote in seats that the party won
predictions %>%
  mutate(winner_vote = pmax(LPC_vote, CPC_vote, NDP_vote, Bloc_vote, Green_vote)) %>%
  group_by(winner) %>%
  summarise(mean = mean(winner_vote),
            pct_05 = quantile(winner_vote, 0.05),
            pct_25 = quantile(winner_vote, 0.25),
            pct_50 = quantile(winner_vote, 0.5),
            pct_75 = quantile(winner_vote, 0.75),
            pct_95 = quantile(winner_vote, 0.9))

predictions %>%
  mutate(winner_vote = pmax(LPC_vote, CPC_vote, NDP_vote, Bloc_vote, Green_vote)) %>%
  dplyr::select(district_code, Liberal = LPC_vote, Conservative = CPC_vote, NDP =  NDP_vote, Bloc = Bloc_vote, winner, winner_vote) %>%
  melt(id.vars = c("district_code", "winner", "winner_vote"), variable.name = "party", value.name = "vote") %>%
  filter(winner != "Green") %>%
  ggplot(aes(x = winner_vote, fill = winner)) +
  facet_wrap(~winner) +
  geom_histogram(col = "black", binwidth = 0.03) +
  scale_fill_manual(name = "Winner", labels = c("Bloc", "Conservative", "Liberal", "NDP"), values = c("#8ECEF9", "blue", "red", "darkorange1")) +
  labs(title = "Distribution of vote in seats won", subtitle = "By party",
       x = "Share of vote", y = "Number of districts")

## Predicted seats
predictions %>%
  group_by(winner) %>%
  summarise(seats = n())

## Predicted seats by region
predictions %>%
  group_by(winner, region) %>%
  summarise(seats = n()) %>%
  spread(winner, seats)

## Predicted seat flips
predictions %>%
  group_by(winner, last_winner) %>%
  summarise(seats = n()) %>%
  spread(winner, seats)

## Seat flips by region
predictions %>%
  filter(winner != last_winner) %>%
  mutate(flip_type = case_when(winner == "Liberal" & last_winner == "Conservative" ~ "CPC to LPC",
                               winner == "Liberal" & last_winner == "NDP" ~ "NDP to LPC",
                               winner == "Liberal" & last_winner == "Green" ~ "Green to LPC",
                               winner == "Liberal" & last_winner == "Bloc" ~ "Bloc to LPC",
                               winner == "Conservative" & last_winner == "Liberal" ~ "LPC to CPC",
                               winner == "Conservative" & last_winner == "NDP" ~ "NDP to CPC",
                               winner == "Conservative" & last_winner == "Green" ~ "Green to CPC",
                               winner == "Conservative" & last_winner == "Bloc" ~ "Bloc to CPC",
                               winner == "NDP" & last_winner == "Liberal" ~ "LPC to NDP",
                               winner == "NDP" & last_winner == "Conservative" ~ "CPC to NDP",
                               winner == "NDP" & last_winner == "Green" ~ "Green to NDP",
                               winner == "NDP" & last_winner == "Bloc" ~ "Bloc to NDP",
                               winner == "Bloc" & last_winner == "Liberal" ~ "LPC to Bloc",
                               winner == "Bloc" & last_winner == "Conservative" ~ "CPC to Bloc",
                               winner == "Bloc" & last_winner == "NDP" ~ "NDP to Bloc")
         ) %>%
  group_by(flip_type, region) %>%
  summarise(n = n()) %>%
  spread(region, n)

## Distribution of seat swings
predictions %>%
  mutate(Liberal = LPC_vote - LPC_lag,
         Conservative = CPC_vote - CPC_lag,
         NDP = NDP_vote - NDP_lag,
         `Bloc Québécois` = Bloc_vote - Bloc_lag,
         Green = Green_vote - Green_lag) %>%
  dplyr::select(district_code, Liberal, Conservative, NDP, `Bloc Québécois`, Green) %>%
  melt(id.vars = "district_code", variable.name = "party", value.name = "pp") %>%
  mutate(pp = 100*pp) %>%
  ggplot(aes(x = pp, fill = party)) +
  facet_wrap(~party, scales = "free_x") +
  geom_histogram(col = "black", binwidth = 1) +
  scale_fill_manual(name = "Party", values = c("red", "blue", "darkorange1", "#8ECEF9", "green4"),
                    labels = c("Liberal", "Conservative", "NDP", "Bloc Québécois", "Green")) +
  labs(title = "Distribution of projected seat swings", subtitle = "By party",
       x = "Swing (pp)", y = "Number of districts")

## Estimated nationwide vote
predictions %>%
  summarise(LPC_votes = sum(LPC_vote*total_votes_2015),
            CPC_votes = sum(CPC_vote*total_votes_2015),
            NDP_votes = sum(NDP_vote*total_votes_2015),
            Bloc_votes = sum(Bloc_vote*total_votes_2015),
            Green_votes = sum(Green_vote*total_votes_2015)) %>%
  mutate(LPC_pct = LPC_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         CPC_pct = CPC_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         NDP_pct = NDP_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         Bloc_pct = Bloc_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         Green_pct = Green_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes)) %>%
  dplyr::select(ends_with("pct"))

## Estimated vote by province
predictions %>%
  group_by(region) %>%
  summarise(LPC_votes = sum(LPC_vote*total_votes_2015),
            CPC_votes = sum(CPC_vote*total_votes_2015),
            NDP_votes = sum(NDP_vote*total_votes_2015),
            Bloc_votes = sum(Bloc_vote*total_votes_2015),
            Green_votes = sum(Green_vote*total_votes_2015)) %>%
  mutate(LPC_pct = LPC_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         CPC_pct = CPC_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         NDP_pct = NDP_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         Bloc_pct = Bloc_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes),
         Green_pct = Green_votes/(LPC_votes+CPC_votes+NDP_votes+Bloc_votes+Green_votes)) %>%
  dplyr::select(region, ends_with("pct"))
