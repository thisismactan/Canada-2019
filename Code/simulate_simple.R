## Run necessaries
source("Code/Modeling/poll_error_variance.R")
source("Code/Modeling/model_error_variance.R")
source("Code/independent_performance.R")
source("Code/poll_average_over_time.R")
source("Code/shape_2019_data.R")

## Clean district keys again for good measure
district_key_2013 <- readr::read_csv("Data/Raw/electoral_districts_key_2013.csv", locale = locale(encoding = "LATIN1"))
district_key_2013 <- district_key_2013 %>%
  mutate(name_english = gsub("[[:digit:]]|[[:space:]][[:digit:]]", "", name_english),
         name_english = gsub("--", "–", name_english),
         name_english = gsub("â€“", "–", name_english), 
         name_english = gsub("--", "–", name_english),
         name_english = gsub("Ã©", "é", name_english),
         name_english = gsub("Ã¨", "è", name_english),
         name_english = gsub("ÃŽ", "Î", name_english),
         name_english = gsub("Ã‰", "É", name_english),
         name_english = gsub("Ã¢", "â", name_english),
         name_english = gsub("Ã´", "ô", name_english),
         name_english = gsub("Ã\\\u008e", "Î", name_english),
         name_english = gsub("Ã\\\u0089", "É", name_english),
         name_english = gsub("Ã®", "î", name_english))

## Simulate draws from national and regional polling
num.iter <- 25000

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

## Compute district polling averages/variances
source("Code/district_polls.R")

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
  Green_district_simulations <- matrix(NA, 338, num.iter)
PPC_district_simulations <- ind_district_simulations <- matrix(0, 338, num.iter)

wilson_raybould_fractions <- rnorm(num.iter, mean = wilson_raybould_mean.logit, sd = 1.1*independent_se) %>% invlogit()
philpott_fractions <- rnorm(num.iter, mean = philpott_mean.logit, sd = 1.1*independent_se) %>% invlogit()
vancouver_granville_row <- 329
markham_stouffville_row <- 164
beauce_row <- 39

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
  Green_district_simulations[, i] <- predict(model_Green.linear, newdata = simulation_data) + simulation_data$Green_lag
}

Sys.time() - start_time

## Adjust ridings known to be pathological

### Vancouver Granville (Jody Wilson-Raybould)
ind_district_simulations[vancouver_granville_row,] <- wilson_raybould_fractions*LPC_district_simulations[vancouver_granville_row,] +
  0.1*NDP_district_simulations[vancouver_granville_row,]
LPC_district_simulations[vancouver_granville_row,] <- (1 - wilson_raybould_fractions)*LPC_district_simulations[vancouver_granville_row,]
NDP_district_simulations[vancouver_granville_row,] <- 0.9*NDP_district_simulations[vancouver_granville_row,]

### Markham--Stouffville (Jane Philpott)
ind_district_simulations[markham_stouffville_row,] <- philpott_fractions*LPC_district_simulations[markham_stouffville_row,] +
  0.1*NDP_district_simulations[markham_stouffville_row,]
LPC_district_simulations[markham_stouffville_row,] <- (1 - philpott_fractions)*LPC_district_simulations[markham_stouffville_row,]
NDP_district_simulations[markham_stouffville_row,] <- 0.9*NDP_district_simulations[markham_stouffville_row,]

### PPC gets leftovers, except in Beauce (Maxime Bernier)
PPC_district_simulations <- 1 - (LPC_district_simulations + CPC_district_simulations + NDP_district_simulations + Bloc_district_simulations + 
                                   Green_district_simulations + ind_district_simulations)
beauce_noise <- rnorm(num.iter, 0, 0.2)
PPC_district_simulations[beauce_row,] <- ((67.02 - 6.64 - 17.09)/(67.02 - 6.64))*CPC_district_simulations[beauce_row,] + beauce_noise
CPC_district_simulations[beauce_row,] <- (17.09/(67.02 - 6.64))*CPC_district_simulations[beauce_row,] - beauce_noise

## Factor in district-level polling

### Calculate variance by district
LPC_district_variance <- rowVars(LPC_district_simulations)
CPC_district_variance <- rowVars(CPC_district_simulations)
NDP_district_variance <- rowVars(NDP_district_simulations)
Bloc_district_variance <- rowVars(Bloc_district_simulations)
Green_district_variance <- rowVars(Green_district_simulations)
PPC_district_variance <- rowVars(PPC_district_simulations)
ind_district_variance <- rowVars(ind_district_simulations)

### Compute rho (weight given to prior)
district_poll_variance <- district_poll_avg$variance
LPC_rho <- district_poll_variance/(district_poll_variance + LPC_district_variance)
CPC_rho <- district_poll_variance/(district_poll_variance + CPC_district_variance)
NDP_rho <- district_poll_variance/(district_poll_variance + NDP_district_variance)
Bloc_rho <- district_poll_variance/(district_poll_variance + Bloc_district_variance)
Green_rho <- district_poll_variance/(district_poll_variance + Green_district_variance)
PPC_rho <- district_poll_variance/(district_poll_variance + PPC_district_variance)
ind_rho <- district_poll_variance/(district_poll_variance + ind_district_variance)

LPC_rho[is.na(LPC_rho)] <- 1
CPC_rho[is.na(CPC_rho)] <- 1
NDP_rho[is.na(NDP_rho)] <- 1
Bloc_rho[is.na(Bloc_rho)] <- 1
Green_rho[is.na(Green_rho)] <- 1
PPC_rho[is.na(PPC_rho)] <- 1
ind_rho[is.na(ind_rho)] <- 1

LPC_rho[beauce_row] <- CPC_rho[beauce_row] <- NDP_rho[beauce_row] <- Bloc_rho[beauce_row] <- Green_rho[beauce_row] <- PPC_rho[beauce_row] <- 
  LPC_rho[vancouver_granville_row] <- CPC_rho[vancouver_granville_row] <- NDP_rho[vancouver_granville_row] <- Bloc_rho[vancouver_granville_row] <- 
  Green_rho[vancouver_granville_row] <- PPC_rho[vancouver_granville_row] <- ind_rho[vancouver_granville_row] <- 0.0001

LPC_district_poll.mat <- matrix(district_poll_avg$LPC_poll, 338, num.iter) + rnorm(338*num.iter, 0, sqrt(district_poll_error_variance))
CPC_district_poll.mat <- matrix(district_poll_avg$CPC_poll, 338, num.iter) + rnorm(338*num.iter, 0, sqrt(district_poll_error_variance))
NDP_district_poll.mat <- matrix(district_poll_avg$NDP_poll, 338, num.iter) + rnorm(338*num.iter, 0, sqrt(district_poll_error_variance))
Bloc_district_poll.mat <- matrix(district_poll_avg$BQ_poll, 338, num.iter) + rnorm(338*num.iter, 0, sqrt(district_poll_error_variance))
Green_district_poll.mat <- matrix(district_poll_avg$GPC_poll, 338, num.iter) + rnorm(338*num.iter, 0, sqrt(district_poll_error_variance))
PPC_district_poll.mat <- matrix(district_poll_avg$PPC_poll, 338, num.iter)
ind_district_poll.mat <- matrix(district_poll_avg$Ind_poll, 338, num.iter) + rnorm(338*num.iter, 0, sqrt(district_poll_error_variance))

LPC_rho.mat <- matrix(LPC_rho, 338, num.iter)
CPC_rho.mat <- matrix(CPC_rho, 338, num.iter)
NDP_rho.mat <- matrix(NDP_rho, 338, num.iter)
Bloc_rho.mat <- matrix(Bloc_rho, 338, num.iter)
Green_rho.mat <- matrix(Green_rho, 338, num.iter)
PPC_rho.mat <- matrix(PPC_rho, 338, num.iter)
ind_rho.mat <- matrix(ind_rho, 338, num.iter)

LPC_district_poll.mat[is.na(LPC_district_poll.mat)] <- CPC_district_poll.mat[is.na(CPC_district_poll.mat)] <-
  NDP_district_poll.mat[is.na(NDP_district_poll.mat)] <- Bloc_district_poll.mat[is.na(Bloc_district_poll.mat)] <-
  Green_district_poll.mat[is.na(Green_district_poll.mat)] <- PPC_district_poll.mat[is.na(PPC_district_poll.mat)] <-
  ind_district_poll.mat[is.na(ind_district_poll.mat)] <- 0

### Weighted average
LPC_district_simulations <- LPC_district_simulations*LPC_rho.mat + LPC_district_poll.mat*(1 - LPC_rho.mat)
CPC_district_simulations <- CPC_district_simulations*CPC_rho.mat + CPC_district_poll.mat*(1 - CPC_rho.mat)
NDP_district_simulations <- NDP_district_simulations*NDP_rho.mat + NDP_district_poll.mat*(1 - NDP_rho.mat)
Bloc_district_simulations <- Bloc_district_simulations*Bloc_rho.mat + Bloc_district_poll.mat*(1 - Bloc_rho.mat)
Green_district_simulations <- Green_district_simulations*Green_rho.mat + Green_district_poll.mat*(1 - Green_rho.mat)
PPC_district_simulations <- PPC_district_simulations*PPC_rho.mat + PPC_district_poll.mat*(1 - PPC_rho.mat)
ind_district_simulations <- ind_district_simulations*ind_rho.mat + ind_district_poll.mat*(1 - ind_rho.mat)

## Vote distributions
LPC_distribution <- tibble(district_code = data_2019.simple$district_code,
                           name_english = district_key_2013$name_english,
                           pct_05.LPC = pmax(0, apply(LPC_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.05)),
                           pct_50.LPC = pmax(0, apply(LPC_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.5)),
                           pct_95.LPC = pmax(0, apply(LPC_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.95)))

CPC_distribution <- tibble(district_code = data_2019.simple$district_code,
                           name_english = district_key_2013$name_english,
                           pct_05.CPC = pmax(0, apply(CPC_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.05)),
                           pct_50.CPC = pmax(0, apply(CPC_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.5)),
                           pct_95.CPC = pmax(0, apply(CPC_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.95)))

NDP_distribution <- tibble(district_code = data_2019.simple$district_code,
                           name_english = district_key_2013$name_english,
                           pct_05.NDP = pmax(0, apply(NDP_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.05)),
                           pct_50.NDP = pmax(0, apply(NDP_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.5)),
                           pct_95.NDP = pmax(0, apply(NDP_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.95)))

Bloc_distribution <- tibble(district_code = data_2019.simple$district_code,
                            name_english = district_key_2013$name_english,
                            pct_05.Bloc = pmax(0, apply(Bloc_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.05)),
                            pct_50.Bloc = pmax(0, apply(Bloc_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.5)),
                            pct_95.Bloc = pmax(0, apply(Bloc_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.95))) %>%
  mutate(pct_05.Bloc = pct_05.Bloc*(1 - (floor(district_code/1000) != 24)),
         pct_50.Bloc = pct_50.Bloc*(1 - (floor(district_code/1000) != 24)),
         pct_95.Bloc = pct_95.Bloc*(1 - (floor(district_code/1000) != 24)))

Green_distribution <- tibble(district_code = data_2019.simple$district_code,
                             name_english = district_key_2013$name_english,
                             pct_05.Green = pmax(0, apply(Green_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.05)),
                             pct_50.Green = pmax(0, apply(Green_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.5)),
                             pct_95.Green = pmax(0, apply(Green_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.95)))

PPC_distribution <- tibble(district_code = data_2019.simple$district_code,
                           name_english = district_key_2013$name_english,
                           pct_05.PPC = pmax(0, apply(PPC_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.05)),
                           pct_50.PPC = pmax(0, apply(PPC_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.5)),
                           pct_95.PPC = pmax(0, apply(PPC_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.95)))

ind_distribution <- tibble(district_code = data_2019.simple$district_code,
                           name_english = district_key_2013$name_english,
                           pct_05.ind = pmax(0, apply(ind_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.05)),
                           pct_50.ind = pmax(0, apply(ind_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.5)),
                           pct_95.ind = pmax(0, apply(ind_district_simulations, MARGIN = 1, FUN = quantile, prob = 0.95)))

## Winners by district by simulation
LPC_wins <- (LPC_district_simulations > CPC_district_simulations) &
  (LPC_district_simulations > NDP_district_simulations) &
  (LPC_district_simulations > Bloc_district_simulations) &
  (LPC_district_simulations > Green_district_simulations) &
  (LPC_district_simulations > PPC_district_simulations) &
  (LPC_district_simulations > ind_district_simulations)

CPC_wins <- (CPC_district_simulations > LPC_district_simulations) &
  (CPC_district_simulations > NDP_district_simulations) &
  (CPC_district_simulations > Bloc_district_simulations) &
  (CPC_district_simulations > Green_district_simulations) &
  (CPC_district_simulations > PPC_district_simulations) &
  (CPC_district_simulations > ind_district_simulations)

NDP_wins <- (NDP_district_simulations > LPC_district_simulations) &
  (NDP_district_simulations > CPC_district_simulations) &
  (NDP_district_simulations > Bloc_district_simulations) &
  (NDP_district_simulations > Green_district_simulations) &
  (NDP_district_simulations > PPC_district_simulations) &
  (NDP_district_simulations > ind_district_simulations)

Bloc_wins <- (Bloc_district_simulations > LPC_district_simulations) &
  (Bloc_district_simulations > CPC_district_simulations) &
  (Bloc_district_simulations > NDP_district_simulations) &
  (Bloc_district_simulations > Green_district_simulations) &
  (Bloc_district_simulations > PPC_district_simulations) &
  (Bloc_district_simulations > ind_district_simulations)

Green_wins <- (Green_district_simulations > LPC_district_simulations) &
  (Green_district_simulations > CPC_district_simulations) &
  (Green_district_simulations > NDP_district_simulations) &
  (Green_district_simulations > Bloc_district_simulations) &
  (Green_district_simulations > PPC_district_simulations) &
  (Green_district_simulations > ind_district_simulations)

PPC_wins <- (PPC_district_simulations > LPC_district_simulations) &
  (PPC_district_simulations > CPC_district_simulations) &
  (PPC_district_simulations > NDP_district_simulations) &
  (PPC_district_simulations > Bloc_district_simulations) &
  (PPC_district_simulations > Green_district_simulations) &
  (PPC_district_simulations > ind_district_simulations)

ind_wins <- (ind_district_simulations > LPC_district_simulations) &
  (ind_district_simulations > CPC_district_simulations) &
  (ind_district_simulations > NDP_district_simulations) &
  (ind_district_simulations > Bloc_district_simulations) &
  (ind_district_simulations > Green_district_simulations) &
  (ind_district_simulations > PPC_district_simulations)

## Win probabilities by district
district_probs <- tibble(district_code = data_2019.simple$district_code,
                         name_english = district_key_2013$name_english,
                         LPC_prob = rowMeans(LPC_wins),
                         CPC_prob = rowMeans(CPC_wins),
                         NDP_prob = rowMeans(NDP_wins),
                         Bloc_prob = rowMeans(Bloc_wins),
                         Green_prob = rowMeans(Green_wins),
                         PPC_prob = rowMeans(PPC_wins),
                         ind_prob = rowMeans(ind_wins))

## Seat counts by simulation
LPC_seats <- colSums(LPC_wins)
CPC_seats <- colSums(CPC_wins)
NDP_seats <- colSums(NDP_wins)
Bloc_seats <- colSums(Bloc_wins)
Green_seats <- colSums(Green_wins)
PPC_seats <- colSums(PPC_wins)
ind_seats <- colSums(ind_wins)

seat_simulations <- tibble(simulation = 1:num.iter,
                           LPC = LPC_seats,
                           CPC = CPC_seats,
                           NDP = NDP_seats,
                           Bloc = Bloc_seats,
                           Green = Green_seats,
                           PPC = PPC_seats,
                           Ind = ind_seats) %>%
  mutate(most_seats = case_when(LPC == pmax(LPC, CPC, NDP, Bloc, Green) ~ "Liberal",
                                CPC == pmax(LPC, CPC, NDP, Bloc, Green) ~ "Conservative",
                                NDP == pmax(LPC, CPC, NDP, Bloc, Green) ~ "NDP",
                                Bloc == pmax(LPC, CPC, NDP, Bloc, Green) ~ "Bloc",
                                Green == pmax(LPC, CPC, NDP, Bloc, Green) ~ "Green"),
         type_of_win = case_when(pmax(LPC, CPC, NDP, Bloc, Green) <= 338/2 ~ "Minority",
                                 pmax(LPC, CPC, NDP, Bloc, Green) > 338/2 ~ "Majority"))

seat_simulations

write_rds(seat_simulations, "Shiny-app/seat_simulations.rds")

## Outcome probabilities
outcome_probs <- seat_simulations %>%
  group_by(most_seats, type_of_win) %>%
  summarise(prob = n()/num.iter) %>%
  spread(type_of_win, prob, fill = 0) %>%
  filter(most_seats %in% c("Conservative", "Liberal", "NDP"))

outcome_probs

write_rds(outcome_probs, "Shiny-app/outcome_probs.rds")

write_rds(Sys.time(), "Shiny-app/update_time.rds")

## Add forecast to timeline
forecast_today <- outcome_probs %>%
  melt(id.vars = c("most_seats"), variable.name = "type", value.name = "prob") %>%
  mutate(outcome = paste(most_seats, tolower(type)),
         date = today()) %>%
  dplyr::select(date, outcome, prob)

forecast_yesterday <- read_csv("Output/forecast_timeline.csv")

## Write the timeline to the output folder and also the Shiny app
bind_rows(forecast_today, forecast_yesterday) %>%
  arrange(desc(date)) %>%
  distinct(date, outcome, .keep_all = TRUE) %>%
  replace_na(list(date = as.Date("2019-10-22"), outcome = "", prob = 0)) %>%
  filter(!grepl("na", outcome)) %>%
  write_csv("Output/forecast_timeline.csv")

bind_rows(forecast_today, forecast_yesterday) %>%
  arrange(desc(date)) %>%
  distinct(date, outcome, .keep_all = TRUE) %>%
  replace_na(list(date = as.Date("2019-10-22"), outcome = "", prob = 0)) %>%
  filter(!grepl("na", outcome)) %>%
  write_csv("Shiny-app/forecast_timeline.csv")

seat_simulations %>%
  group_by(most_seats) %>%
  summarise(prob = n()/num.iter)

## Forecast over time
forecast_timeline <- read_csv("Output/forecast_timeline.csv") %>%
  replace(is.na(.), 0)

forecast_timeline %>%
  ggplot(aes(x = date, y = prob, col = outcome)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2019-10-21")) +
  scale_colour_manual(name = "Outcome", values = c("blue", "#AAAAFF", "red", "#FFAAAA", "darkorange1", "#FFBB77"),
                      labels = c("Conservative majority", "Conservative minority", "Liberal majority", "Liberal minority", 
                                 "NDP majority", "NDP minority")) +
  scale_x_date(limits = as.Date(c("2019-07-15", "2019-10-21")), breaks = "1 week", date_labels = "%b %e") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Forecast over time", x = "Date", y = "Probability")

## Vote distribution by party
national_poll_sims %>%
  as.data.frame() %>%
  as.tbl() %>%
  dplyr::select(Liberal = V1, Conservative = V2, NDP = V3, Bloc = V4, Green = V5, `People's` = V6) %>%
  melt(variable.name = "party", value.name = "pct") %>%
  group_by(party) %>%
  summarise(mean = mean(pct),
            pct_05 = quantile(pct, 0.05),
            pct_50 = quantile(pct, 0.5),
            pct_95 = quantile(pct, 0.95))

## Seat distributions by party
seat_simulations %>%
  reshape2::melt(id.vars = c("simulation", "most_seats", "type_of_win"), variable.name = "party", value.name = "seats") %>%
  group_by(party) %>%
  summarise(mean = mean(seats),
            pct_05 = quantile(seats, 0.05),
            pct_25 = quantile(seats, 0.25),
            pct_50 = quantile(seats, 0.5),
            pct_75 = quantile(seats, 0.75),
            pct_95 = quantile(seats, 0.9))

seat_simulations %>%
  dplyr::select(-most_seats, -type_of_win) %>%
  melt(id.var = "simulation", variable.name = "party", value.name = "seats") %>%
  filter(party %in% c("LPC", "CPC", "NDP")) %>%
  ggplot() +
  geom_histogram(aes(x = seats, y = ..density.., fill = party),
                 binwidth = 3, alpha = 2/3, position = "identity", col = "black") +
  geom_vline(xintercept = 170, size = 1) +
  geom_text(data = data.frame(x = 200,
                              y = 0.02,
                              label = "170 seats needed\n for a majority"),
            aes(x = x, y = y, label = label), size = 3) +
  scale_fill_manual(name = "Party", values = c("red", "blue", "darkorange1"), labels = c("Liberal", "Conservative", "NDP")) +
  scale_x_continuous(breaks = seq(0, 300, by = 50)) +
  labs(title = "Forecast distribution of seat counts by party", x = "Seats", y = "Probability",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

## Write data with rho and actual poll average to the Shiny folder
data_2019.simple %>%
  mutate(LPC_rho = LPC_rho,
         CPC_rho = CPC_rho,
         NDP_rho = NDP_rho,
         Bloc_rho = Bloc_rho,
         Green_rho = Green_rho,
         PPC_rho = PPC_rho,
         ind_rho = ind_rho) %>%
  left_join(district_poll_avg, by = "district_code") %>%
  write_rds("Shiny-app/data_2019.rds")
