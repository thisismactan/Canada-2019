## Run necessaries
source("Code/Modeling/poll_error_variance.R")
source("Code/Modeling/model_error_variance.R")
source("Code/poll_average.R")
source("Code/shape_2019_data.R")

## Simulate draws from national and regional polling
num.iter <- 10000

set.seed(2019)
national_means <- national_polls.adjusted %>% 
  melt(id.vars = c("pollster", "date", "age", "MOE", "n", "mode", "IVR", "weight")) %>%
  filter(variable %in% c("LPC", "CPC", "NDP", "BQ", "GPC", "PPC")) %>%
  group_by(party = variable) %>%
  summarise(average = Hmisc::wtd.mean(value, weights = weight, na.rm = TRUE)) %>%
  pull(average)

atlantic_means <- region_wtd_mean(atlantic_polls)
quebec_means <- quebec_polls %>%
  summarise(LPC = wtd.mean(LPC, weights = weight),
            CPC = wtd.mean(CPC, weights = weight),
            NDP = wtd.mean(NDP, weights = weight),
            BQ = wtd.mean(BQ, weights = weight),
            GPC = wtd.mean(GPC, weights = weight),
            PPC = wtd.mean(PPC, weights = weight)) %>%
  t() %>%
  t() %>%
  as.vector()
ontario_means <- region_wtd_mean(ontario_polls)
prairie_means <- region_wtd_mean(prairie_polls)
alberta_means <- region_wtd_mean(alberta_polls)
bc_means <- region_wtd_mean(bc_polls)

national_poll_errors <- rmvn(num.iter, rep(0, 5), poll_error_covariance) %>%
  cbind(0)

## Variation in polling error
national_poll_sims <- (rmvn(num.iter, national_means, national_polls_covariance) + national_poll_errors)/100
atlantic_poll_sims <- (rmvn(num.iter, atlantic_means, atlantic_polls_covariance) + national_poll_errors[, c(1:3, 5:6)])/100
quebec_poll_sims <- (rmvn(num.iter, quebec_means, quebec_polls_covariance) + national_poll_errors)/100
ontario_poll_sims <- (rmvn(num.iter, ontario_means, ontario_polls_covariance) + national_poll_errors[, c(1:3, 5:6)])/100
prairie_poll_sims <- (rmvn(num.iter, prairie_means, prairie_polls_covariance) + national_poll_errors[, c(1:3, 5:6)])/100
alberta_poll_sims <- (rmvn(num.iter, alberta_means, alberta_polls_covariance) + national_poll_errors[, c(1:3, 5:6)])/100
bc_poll_sims <- (rmvn(num.iter, bc_means, bc_polls_covariance) + national_poll_errors[, c(1:3, 5:6)])/100
frigid_northlands_sims <- (rmvn(num.iter, frigid_northlands_means, frigid_northlands_covariance) + national_poll_errors[, c(1:3, 5)])/100

LPC_district_simulations <- CPC_district_simulations <- NDP_district_simulations <- Bloc_district_simulations <- 
  Green_district_simulations <-matrix(NA, 338, num.iter)

## Simulation
start_time <- Sys.time()
for(i in 1:num.iter) {
  if(i %% 1000 == 0) {
    cat("Simulations run:", i, "\n")
  }
  simulation_data <- data_2019.simple %>%
    ## Replace national vote
    mutate(LPC_nation = national_poll_sims[i,1],
           CPC_nation = national_poll_sims[i,2],
           NDP_nation = national_poll_sims[i,3],
           Bloc_nation = national_poll_sims[i,4],
           Green_nation = national_poll_sims[i,5]) %>%
    
    ## Now replace regional vote
    mutate(LPC_region = case_when(region == "Atlantic" ~ atlantic_poll_sims[i,1],
                                  region == "Quebec" ~ quebec_poll_sims[i,1],
                                  region == "Ontario" ~ ontario_poll_sims[i,1],
                                  region == "Prairie" ~ prairie_poll_sims[i,1],
                                  region == "Alberta" ~ alberta_poll_sims[i,1],
                                  region == "British Columbia" ~ bc_poll_sims[i,1],
                                  region == "The frigid northlands" ~ frigid_northlands_sims[i,1]),
           CPC_region = case_when(region == "Atlantic" ~ atlantic_poll_sims[i,2],
                                  region == "Quebec" ~ quebec_poll_sims[i,2],
                                  region == "Ontario" ~ ontario_poll_sims[i,2],
                                  region == "Prairie" ~ prairie_poll_sims[i,2],
                                  region == "Alberta" ~ alberta_poll_sims[i,2],
                                  region == "British Columbia" ~ bc_poll_sims[i,2],
                                  region == "The frigid northlands" ~ frigid_northlands_sims[i,2]),
           NDP_region = case_when(region == "Atlantic" ~ atlantic_poll_sims[i,3],
                                  region == "Quebec" ~ quebec_poll_sims[i,3],
                                  region == "Ontario" ~ ontario_poll_sims[i,3],
                                  region == "Prairie" ~ prairie_poll_sims[i,3],
                                  region == "Alberta" ~ alberta_poll_sims[i,3],
                                  region == "British Columbia" ~ bc_poll_sims[i,3],
                                  region == "The frigid northlands" ~ frigid_northlands_sims[i,3]),
           Bloc_region = case_when(region == "Quebec" ~ quebec_poll_sims[i,4],
                                   region != "Quebec" ~ 0),
           Green_region = case_when(region == "Atlantic" ~ atlantic_poll_sims[i,4],
                                    region == "Quebec" ~ quebec_poll_sims[i,5],
                                    region == "Ontario" ~ ontario_poll_sims[i,4],
                                    region == "Prairie" ~ prairie_poll_sims[i,4],
                                    region == "Alberta" ~ alberta_poll_sims[i,4],
                                    region == "British Columbia" ~ bc_poll_sims[i,4],
                                    region == "The frigid northlands" ~ frigid_northlands_sims[i,4]))
  
  ## Make predictions for this simulation
  LPC_district_simulations[, i] <- predict(model_LPC.linear, newdata = simulation_data)
  CPC_district_simulations[, i] <- predict(model_CPC.linear, newdata = simulation_data)
  NDP_district_simulations[, i] <- predict(model_NDP.linear, newdata = simulation_data)
  Bloc_district_simulations[, i] <- predict(model_Bloc.linear, newdata = simulation_data)
  Green_district_simulations[, i] <- predict(model_Green.linear, newdata = simulation_data)
}

Sys.time() - start_time

## Compute summary statistics
## 5th percentile
pct_05.LPC <- compute_district_stats(LPC_district_simulations, FUN = quantile, prob = 0.05)
pct_05.CPC <- compute_district_stats(CPC_district_simulations, FUN = quantile, prob = 0.05)
pct_05.NDP <- compute_district_stats(NDP_district_simulations, FUN = quantile, prob = 0.05)
pct_05.Bloc <- compute_district_stats(Bloc_district_simulations, FUN = quantile, prob = 0.05)
pct_05.Green <- compute_district_stats(Green_district_simulations, FUN = quantile, prob = 0.05)

## 25th percentile
pct_25.LPC <- compute_district_stats(LPC_district_simulations, FUN = quantile, prob = 0.25)
pct_25.CPC <- compute_district_stats(CPC_district_simulations, FUN = quantile, prob = 0.25)
pct_25.NDP <- compute_district_stats(NDP_district_simulations, FUN = quantile, prob = 0.25)
pct_25.Bloc <- compute_district_stats(Bloc_district_simulations, FUN = quantile, prob = 0.25)
pct_25.Green <- compute_district_stats(Green_district_simulations, FUN = quantile, prob = 0.25)

## 50th percentile
pct_50.LPC <- compute_district_stats(LPC_district_simulations, FUN = median)
pct_50.CPC <- compute_district_stats(CPC_district_simulations, FUN = median)
pct_50.NDP <- compute_district_stats(NDP_district_simulations, FUN = median)
pct_50.Bloc <- compute_district_stats(Bloc_district_simulations, FUN = median)
pct_50.Green <- compute_district_stats(Green_district_simulations, FUN = median)

## 75th percentile
pct_75.LPC <- compute_district_stats(LPC_district_simulations, FUN = quantile, prob = 0.75)
pct_75.CPC <- compute_district_stats(CPC_district_simulations, FUN = quantile, prob = 0.75)
pct_75.NDP <- compute_district_stats(NDP_district_simulations, FUN = quantile, prob = 0.75)
pct_75.Bloc <- compute_district_stats(Bloc_district_simulations, FUN = quantile, prob = 0.75)
pct_75.Green <- compute_district_stats(Green_district_simulations, FUN = quantile, prob = 0.75)

## 95th percentile
pct_95.LPC <- compute_district_stats(LPC_district_simulations, FUN = quantile, prob = 0.95)
pct_95.CPC <- compute_district_stats(CPC_district_simulations, FUN = quantile, prob = 0.95)
pct_95.NDP <- compute_district_stats(NDP_district_simulations, FUN = quantile, prob = 0.95)
pct_95.Bloc <- compute_district_stats(Bloc_district_simulations, FUN = quantile, prob = 0.95)
pct_95.Green <- compute_district_stats(Green_district_simulations, FUN = quantile, prob = 0.95)

#### Vote distributions ####

## Liberals
LPC_distribution <- 

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
  filter(region == "The frigid northlands") %>%
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