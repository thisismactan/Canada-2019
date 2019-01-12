#### Testing random forest models ####
source("Code/historical_results.R")
set.seed(2019)

#### LOOCV on all years, non-Quebec ####
results_anglo <- historical_results.district %>%
  filter(province != "Quebec", year != 2004) 
n <- nrow(results_anglo)
LPC_error <- CPC_error <- NDP_error <- Green_error <- rep(NA, n)

for(i in 1:n) {
  # Train/test split
  train <- results_anglo[-i,]
  test <- results_anglo[i,]
  
  # Fit random forest model (default options)
  rf.LPC <- randomForest(LPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+LPC_nation+LPC_region+LPC_nation_lag+LPC_region_lag+
                           LPC_funds+LPC_funds_frac, data = train)
  rf.CPC <- randomForest(CPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+CPC_nation+CPC_region+CPC_nation_lag+CPC_region_lag+
                           CPC_funds+CPC_funds_frac, data = train)
  rf.NDP <- randomForest(NDP~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+NDP_nation+NDP_region+NDP_nation_lag+NDP_region_lag+
                           NDP_funds+NDP_funds_frac, data = train)
  rf.Green <- randomForest(Green~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Green_nation+Green_region+Green_nation_lag+Green_region_lag+
                             Green_funds+Green_funds_frac, data = train)
  
  # Make test prediction and compute error
  test <- test %>%
    mutate(LPC.pred = predict(rf.LPC, newdata = .), LPC.error = LPC.pred - LPC,
           CPC.pred = predict(rf.CPC, newdata = .), CPC.error = CPC.pred - CPC,
           NDP.pred = predict(rf.NDP, newdata = .), NDP.error = NDP.pred - NDP,
           Green.pred = predict(rf.Green, newdata = .), Green.error = Green.pred - Green)
  
  LPC_error[i] <- test$LPC.error[1]
  CPC_error[i] <- test$CPC.error[1]
  NDP_error[i] <- test$NDP.error[1]
  Green_error[i] <- test$Green.error[1]
}

rf.errors_anglo <- results_anglo %>%
  mutate(LPC_error = LPC_error,
         CPC_error = CPC_error,
         NDP_error = NDP_error,
         Green_error = Green_error) %>%
  dplyr::select(district_code, name_english, year, incumbent, province, region, LPC_error, CPC_error, NDP_error, Green_error)

## Error summary stats
# RMSE by party
rf.errors_anglo %>%
  summarise(n = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)))

# RMSE by party, region
rf.errors_anglo %>%
  group_by(region) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)))

# RMSE by party, year
rf.errors_anglo %>%
  group_by(year) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)))

## Density plots 
# Party, by region
rf.errors_anglo %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", alpha = 0.5, binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = c("red", "blue", "orange4", "green4"), labels = c("LPC", "CPC", "NDP", "Green")) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 1",
       x = "Error", y = "Density")

ggsave(filename = "Miscellanea/Model graphs/rf_errors_region_anglo.png", width = 16, height = 9)

# Party, by year
rf.errors_anglo %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~year) +
  geom_histogram(col = "black", alpha = 0.5, binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = c("red", "blue", "orange4", "green4"), labels = c("LPC", "CPC", "NDP", "Green")) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 1",
       x = "Error", y = "Density")

ggsave(filename = "Miscellanea/Model graphs/rf_errors_year_anglo.png", width = 16, height = 5)

#### LOOCV on all years, all districts ####
results <- historical_results.district %>%
  filter(year != 2004) %>%
  mutate_at(vars(contains("funds")), function(x) {
    x[is.na(x)] <- 0
    return(x)
  }) %>%
  mutate(Quebec = (province == "Quebec"))
  
results$Bloc[is.na(results$Bloc)] <- 0
results$Bloc[is.na(results$Bloc)] <- 0

n <- nrow(results)
LPC_error <- CPC_error <- NDP_error <- Green_error <- Bloc_error <- rep(NA, n)

for(i in 1:n) {
  # Train/test split
  train <- results[-i,]
  test <- results[i,]
  
  # Fit random forest model (default options)
  rf.LPC <- randomForest(LPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+LPC_nation+LPC_region+LPC_nation_lag+LPC_region_lag+
                           LPC_funds+LPC_funds_frac, data = train)
  rf.CPC <- randomForest(CPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+CPC_nation+CPC_region+CPC_nation_lag+CPC_region_lag+
                           CPC_funds+CPC_funds_frac, data = train)
  rf.NDP <- randomForest(NDP~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+NDP_nation+NDP_region+NDP_nation_lag+NDP_region_lag+
                           NDP_funds+NDP_funds_frac, data = train)
  rf.Green <- randomForest(Green~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+Green_nation+Green_region+Green_nation_lag+
                             Green_region_lag+Green_funds+Green_funds_frac, data = train)
  rf.Bloc <- randomForest(Bloc~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+Bloc_nation+Bloc_region+Bloc_nation_lag+Bloc_region_lag+
                           Bloc_funds+Bloc_funds_frac, data = train)
  
  # Make test prediction and compute error
  test <- test %>%
    mutate(LPC.pred = predict(rf.LPC, newdata = .), LPC.error = LPC.pred - LPC,
           CPC.pred = predict(rf.CPC, newdata = .), CPC.error = CPC.pred - CPC,
           NDP.pred = predict(rf.NDP, newdata = .), NDP.error = NDP.pred - NDP,
           Green.pred = predict(rf.Green, newdata = .), Green.error = Green.pred - Green,
           Bloc.pred = predict(rf.Bloc, newdata = .), Bloc.error = Bloc.pred - Bloc)
  
  LPC_error[i] <- test$LPC.error[1]
  CPC_error[i] <- test$CPC.error[1]
  NDP_error[i] <- test$NDP.error[1]
  Green_error[i] <- test$Green.error[1]
  Bloc_error[i] <- test$Bloc.error[1]
}

rf.errors <- results %>%
  mutate(LPC_error = LPC_error,
         CPC_error = CPC_error,
         NDP_error = NDP_error,
         Green_error = Green_error,
         Bloc_error = Bloc_error) %>%
  dplyr::select(district_code, name_english, year, incumbent, province, region, LPC_error, CPC_error, NDP_error, Green_error, Bloc_error)

## Error summary stats
# RMSE by party
rf.errors %>%
  summarise(n = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, region
rf.errors %>%
  group_by(region) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, year
rf.errors %>%
  group_by(year) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

## Density plots 
# Party, by region
rf.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", alpha = 0.5, binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = c("red", "blue", "orange4", "green4"), labels = c("LPC", "CPC", "NDP", "Green")) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 1",
       x = "Error", y = "Density")

ggsave(filename = "Miscellanea/Model graphs/rf_errors_region.png", width = 16, height = 9)

# Party, by year
rf.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~year) +
  geom_histogram(col = "black", alpha = 0.5, binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = c("red", "blue", "orange4", "green4"), labels = c("LPC", "CPC", "NDP", "Green")) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 1",
       x = "Error", y = "Density")

ggsave(filename = "Miscellanea/Model graphs/rf_errors_year.png", width = 16, height = 5)

#### LOOCV on all years, all districts + population predictors
results <- historical_results.district %>%
  filter(year != 2004) %>%
  mutate_at(vars(contains("funds")), function(x) {
    x[is.na(x)] <- 0
    return(x)
  }) %>%
  mutate(Quebec = (province == "Quebec"))

results$Bloc[is.na(results$Bloc)] <- 0
results$Bloc[is.na(results$Bloc)] <- 0

n <- nrow(results)
LPC_error <- CPC_error <- NDP_error <- Green_error <- Bloc_error <- rep(NA, n)

for(i in 1:n) {
  # Train/test split
  train <- results[-i,]
  test <- results[i,]
  
  # Fit random forest model (default options)
  rf.LPC <- randomForest(LPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+LPC_nation+LPC_region+LPC_nation_lag+LPC_region_lag+
                           LPC_funds+LPC_funds_frac, data = train)
  rf.CPC <- randomForest(CPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+CPC_nation+CPC_region+CPC_nation_lag+CPC_region_lag+
                           CPC_funds+CPC_funds_frac, data = train)
  rf.NDP <- randomForest(NDP~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+NDP_nation+NDP_region+NDP_nation_lag+NDP_region_lag+
                           NDP_funds+NDP_funds_frac, data = train)
  rf.Green <- randomForest(Green~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+Green_nation+Green_region+Green_nation_lag+
                             Green_region_lag+Green_funds+Green_funds_frac, data = train)
  rf.Bloc <- randomForest(Bloc~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+Bloc_nation+Bloc_region+Bloc_nation_lag+Bloc_region_lag+
                            Bloc_funds+Bloc_funds_frac, data = train)
  
  # Make test prediction and compute error
  test <- test %>%
    mutate(LPC.pred = predict(rf.LPC, newdata = .), LPC.error = LPC.pred - LPC,
           CPC.pred = predict(rf.CPC, newdata = .), CPC.error = CPC.pred - CPC,
           NDP.pred = predict(rf.NDP, newdata = .), NDP.error = NDP.pred - NDP,
           Green.pred = predict(rf.Green, newdata = .), Green.error = Green.pred - Green,
           Bloc.pred = predict(rf.Bloc, newdata = .), Bloc.error = Bloc.pred - Bloc)
  
  LPC_error[i] <- test$LPC.error[1]
  CPC_error[i] <- test$CPC.error[1]
  NDP_error[i] <- test$NDP.error[1]
  Green_error[i] <- test$Green.error[1]
  Bloc_error[i] <- test$Bloc.error[1]
}

rf.errors <- results %>%
  mutate(LPC_error = LPC_error,
         CPC_error = CPC_error,
         NDP_error = NDP_error,
         Green_error = Green_error,
         Bloc_error = Bloc_error) %>%
  dplyr::select(district_code, name_english, year, incumbent, province, region, LPC_error, CPC_error, NDP_error, Green_error, Bloc_error)

## Error summary stats
# RMSE by party
rf.errors %>%
  summarise(n = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, region
rf.errors %>%
  group_by(region) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, year
rf.errors %>%
  group_by(year) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

## Density plots 
# Party, by region
rf.errors %>%
  filter(region != "The frigid northlands") %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", alpha = 0.7, binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = c("red", "blue", "darkorange1", "green4", "#8ECEF9"), labels = c("LPC", "CPC", "NDP", "Green", "Bloc")) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 1",
       x = "Error", y = "Density")

ggsave(filename = "Miscellanea/Model graphs/rf_errors_region.png", width = 20, height = 12)

# Party, by year
rf.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~year) +
  geom_histogram(col = "black", alpha = 0.7, binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = c("red", "blue", "darkorange1", "green4", "#8ECEF9"), labels = c("LPC", "CPC", "NDP", "Green", "Bloc")) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 1",
       x = "Error", y = "Density")

ggsave(filename = "Miscellanea/Model graphs/rf_errors_year.png", width = 20, height = 7)
