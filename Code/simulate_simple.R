## Run necessaries
source("Code/Modeling/poll_error_variance.R")
source("Code/Modeling/model_error_variance.R")
source("Code/poll_average.R")
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

## Vote distributions
LPC_distribution <- tibble(district_code = data_2019.simple$district_code,
                           name_english = district_key_2013$name_english,
                           pct_05.LPC,
                           pct_25.LPC,
                           pct_50.LPC,
                           pct_75.LPC,
                           pct_95.LPC)

CPC_distribution <- tibble(district_code = data_2019.simple$district_code,
                           name_english = district_key_2013$name_english,
                           pct_05.CPC,
                           pct_25.CPC,
                           pct_50.CPC,
                           pct_75.CPC,
                           pct_95.CPC)

NDP_distribution <- tibble(district_code = data_2019.simple$district_code,
                           name_english = district_key_2013$name_english,
                           pct_05.NDP,
                           pct_25.NDP,
                           pct_50.NDP,
                           pct_75.NDP,
                           pct_95.NDP)

Bloc_distribution <- tibble(district_code = data_2019.simple$district_code,
                            name_english = district_key_2013$name_english,
                            pct_05.Bloc,
                            pct_25.Bloc,
                            pct_50.Bloc,
                            pct_75.Bloc,
                            pct_95.Bloc) %>%
  mutate(pct_05.Bloc = pct_05.Bloc*(1 - (floor(district_code/1000) != 24)),
         pct_25.Bloc = pct_25.Bloc*(1 - (floor(district_code/1000) != 24)),
         pct_50.Bloc = pct_50.Bloc*(1 - (floor(district_code/1000) != 24)),
         pct_75.Bloc = pct_75.Bloc*(1 - (floor(district_code/1000) != 24)),
         pct_95.Bloc = pct_95.Bloc*(1 - (floor(district_code/1000) != 24)))

Green_distribution <- tibble(district_code = data_2019.simple$district_code,
                             name_english = district_key_2013$name_english,
                             pct_05.Green,
                             pct_25.Green,
                             pct_50.Green,
                             pct_75.Green,
                             pct_95.Green)

## Winners by district by simulation
LPC_wins <- (LPC_district_simulations > CPC_district_simulations) &
  (LPC_district_simulations > NDP_district_simulations) &
  (LPC_district_simulations > Bloc_district_simulations) &
  (LPC_district_simulations > Green_district_simulations)

CPC_wins <- (CPC_district_simulations > LPC_district_simulations) &
  (CPC_district_simulations > NDP_district_simulations) &
  (CPC_district_simulations > Bloc_district_simulations) &
  (CPC_district_simulations > Green_district_simulations)

NDP_wins <- (NDP_district_simulations > LPC_district_simulations) &
  (NDP_district_simulations > CPC_district_simulations) &
  (NDP_district_simulations > Bloc_district_simulations) &
  (NDP_district_simulations > Green_district_simulations)

Bloc_wins <- (Bloc_district_simulations > LPC_district_simulations) &
  (Bloc_district_simulations > CPC_district_simulations) &
  (Bloc_district_simulations > NDP_district_simulations) &
  (Bloc_district_simulations > Green_district_simulations)

Green_wins <- (Green_district_simulations > LPC_district_simulations) &
  (Green_district_simulations > CPC_district_simulations) &
  (Green_district_simulations > NDP_district_simulations) &
  (Green_district_simulations > Bloc_district_simulations)

## Win probabilities by district
district_probs <- tibble(district_code = data_2019.simple$district_code,
                         name_english = district_key_2013$name_english,
                         LPC_prob = rowMeans(LPC_wins),
                         CPC_prob = rowMeans(CPC_wins),
                         NDP_prob = rowMeans(NDP_wins),
                         Bloc_prob = rowMeans(Bloc_wins),
                         Green_prob = rowMeans(Green_wins))

## Seat counts by simulation
LPC_seats <- colSums(LPC_wins)
CPC_seats <- colSums(CPC_wins)
NDP_seats <- colSums(NDP_wins)
Bloc_seats <- colSums(Bloc_wins)
Green_seats <- colSums(Green_wins)

seat_simulations <- tibble(simulation = 1:num.iter,
                           LPC = LPC_seats,
                           CPC = CPC_seats,
                           NDP = NDP_seats,
                           Bloc = Bloc_seats,
                           Green = Green_seats) %>%
  mutate(most_seats = case_when(LPC == pmax(LPC, CPC, NDP, Bloc, Green) ~ "Liberal",
                                CPC == pmax(LPC, CPC, NDP, Bloc, Green) ~ "Conservative",
                                NDP == pmax(LPC, CPC, NDP, Bloc, Green) ~ "NDP",
                                Bloc == pmax(LPC, CPC, NDP, Bloc, Green) ~ "Bloc",
                                Green == pmax(LPC, CPC, NDP, Bloc, Green) ~ "Green"),
         type_of_win = case_when(pmax(LPC, CPC, NDP, Bloc, Green) <= 338/2 ~ "Minority",
                                 pmax(LPC, CPC, NDP, Bloc, Green) > 338/2 ~ "Majority"))

## Outcome probabilities
seat_simulations %>%
  group_by(most_seats, type_of_win) %>%
  summarise(prob = n()/num.iter) %>%
  spread(type_of_win, prob)

seat_simulations %>%
  group_by(most_seats) %>%
  summarise(prob = n()/num.iter)

## Seat distributions by party
