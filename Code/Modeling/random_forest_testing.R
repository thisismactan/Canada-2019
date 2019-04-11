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
  scale_fill_manual(name = "Party", values = national_colors[1:4], labels = national_parties[1:4]) +
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
  scale_fill_manual(name = "Party", values = national_colors[1:4], labels = national_parties[1:4]) +
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

write.csv(rf.errors, file = "Output/Model testing/rf_errors.csv", row.names = FALSE)

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
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 1",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/rf_errors_region.png", width = 16, height = 9)

# Party, by year
rf.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~year) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and year",
       subtitle = "Random forest 1",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/rf_errors_year.png", width = 16, height = 5)

#### LOOCV cross-validation on all years, all districts + demographic predictors ####
results <- historical_results.district %>%
  filter(year != 2004) %>%
  mutate_at(vars(contains("funds")), function(x) {
    x[is.na(x)] <- 0
    return(x)
  }) %>%
  mutate(Quebec = (province == "Quebec"),
         Atlantic = (region == "Atlantic"))

results$Bloc[is.na(results$Bloc)] <- 0
results$Bloc[is.na(results$Bloc)] <- 0

n <- nrow(results)

## Tuning
tunegrid <- expand.grid(mtry = 5:20)
control <- trainControl(method = "cv", number = 10)
set.seed(2019)
rf.LPC_tune <- train(LPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+LPC_nation+LPC_region+LPC_nation_lag+LPC_region_lag+
                       LPC_funds+LPC_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, data = results,
                     method = "rf", metric = "RMSE", tuneGrid = tunegrid, trControl = control) # mtry = 9
rf.CPC_tune <- train(CPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+CPC_nation+CPC_region+CPC_nation_lag+CPC_region_lag+
                       CPC_funds+CPC_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, data = results,
                     method = "rf", metric = "RMSE", tuneGrid = tunegrid, trControl = control) # mtry = 9
rf.NDP_tune <- train(NDP~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+NDP_nation+NDP_region+NDP_nation_lag+NDP_region_lag+
                       NDP_funds+NDP_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, data = results,
                     method = "rf", metric = "RMSE", tuneGrid = tunegrid, trControl = control) # mtry = 8
rf.Green_tune <- train(Green~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+Green_nation+Green_region+Green_nation_lag+
                        Green_region_lag+Green_funds+Green_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, data = results,
                     method = "rf", metric = "RMSE", tuneGrid = tunegrid, trControl = control) # mtry = 17
rf.Bloc_tune <- train(Bloc~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+Bloc_nation+Bloc_region+Bloc_nation_lag+Bloc_region_lag+
                         Bloc_funds+Bloc_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, data = results,
                     method = "rf", metric = "RMSE", tuneGrid = tunegrid, trControl = control) # mtry = 12

LPC_error <- CPC_error <- NDP_error <- Green_error <- Bloc_error <- rep(NA, n)

set.seed(2019)
for(i in 1:n) {
  cat("District ", results$district_code[i], ": ", results$name_english[i], " (", results$year[i], ")", "\n", sep = "")
  # Train/test split
  train <- results[-i,]
  test <- results[i,]
  
  # Fit random forest model (after tuning)
  rf.LPC <- randomForest(LPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+LPC_nation+LPC_region+LPC_nation_lag+LPC_region_lag+
                           LPC_funds+LPC_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, data = train, mtry = 9)
  rf.CPC <- randomForest(CPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+CPC_nation+CPC_region+CPC_nation_lag+CPC_region_lag+
                           CPC_funds+CPC_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, data = train, mtry = 9)
  rf.NDP <- randomForest(NDP~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+NDP_nation+NDP_region+NDP_nation_lag+NDP_region_lag+
                           NDP_funds+NDP_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, data = train, mtry = 8)
  rf.Green <- randomForest(Green~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+Green_nation+Green_region+Green_nation_lag+
                             Green_region_lag+Green_funds+Green_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, data = train,
                           mtry = 17)
  rf.Bloc <- randomForest(Bloc~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+Bloc_nation+Bloc_region+Bloc_nation_lag+Bloc_region_lag+
                            Bloc_funds+Bloc_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, data = train, mtry = 12)
  
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

rf_demographics.errors <- results %>%
  mutate(LPC_error = LPC_error,
         CPC_error = CPC_error,
         NDP_error = NDP_error,
         Green_error = Green_error,
         Bloc_error = Bloc_error) %>%
  dplyr::select(district_code, name_english, year, incumbent, province, region, LPC_error, CPC_error, NDP_error, Green_error, Bloc_error)

write.csv(rf_demographics.errors, file = "Output/Model testing/rf_demographics_errors.csv", row.names = FALSE) 

## Error summary stats
# RMSE by party
rf_demographics.errors %>%
  summarise(n = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, region
rf_demographics.errors %>%
  group_by(region) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, year
rf_demographics.errors %>%
  group_by(year) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

## Density plots 
# Party, overall
rf_demographics.errors %>%
  filter(region != "The frigid northlands") %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 1",
       x = "Error", y = "Density")

# Party, by region
rf_demographics.errors %>%
  filter(region != "The frigid northlands") %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 1",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/rf_demographics_errors_region.png", width = 20, height = 12)

# Party, by year
rf_demographics.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~year) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 1",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/rf_demographics_errors_year.png", width = 20, height = 7)

#### LOOCV after feature pruning and mtry tuning ####
results <- historical_results.district %>%
  filter(year != 2004) %>%
  mutate_at(vars(contains("funds")), function(x) {
    x[is.na(x)] <- 0
    return(x)
  }) %>%
  mutate(Quebec = (province == "Quebec"),
         Atlantic = (region == "Atlantic"))

results$Bloc[is.na(results$Bloc)] <- 0
results$Bloc[is.na(results$Bloc)] <- 0

n <- nrow(results)
LPC_error <- CPC_error <- NDP_error <- Green_error <- Bloc_error <- rep(NA, n)

set.seed(2019)
for(i in 1:n) {
  cat("District ", results$district_code[i], ": ", results$name_english[i], " (", results$year[i], ")", "\n", sep = "")
  # Train/test split
  train <- results[-i,]
  test <- results[i,]
  
  # Fit random forest model (after tuning)
  rf2.LPC <- randomForest(LPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+LPC_nation+LPC_region+LPC_nation_lag+LPC_region_lag+
                            LPC_funds+LPC_funds_frac+pop_growth_rate+educ_university+minority, 
                          data = results, mtry = 5, localImp = TRUE)
  rf2.CPC <- randomForest(CPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+CPC_nation+CPC_region+CPC_nation_lag+CPC_region_lag+CPC_funds+
                            CPC_funds_frac+educ_university+minority, 
                          data = results, mtry = 5, localImp = TRUE)
  rf2.NDP <- randomForest(NDP~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+NDP_nation+NDP_region+NDP_nation_lag+NDP_region_lag+
                            NDP_funds+NDP_funds_frac+pop_growth_rate+educ_university+minority, 
                          data = results, mtry = 6, localImp = TRUE)
  rf2.Green <- randomForest(Green~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Green_nation+Green_region+Green_nation_lag+Green_region_lag+
                              Green_funds+Green_funds_frac+pop_growth_rate+sex_female+minority, 
                            data = results, mtry = 14, localImp = TRUE)
  rf2.Bloc <- randomForest(Bloc~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Bloc_nation+Bloc_region+Bloc_nation_lag+Bloc_region_lag+
                             pop_growth_rate+educ_university, 
                           data = results, mtry = 5, localImp = TRUE)
  
  # Make test prediction and compute error
  test <- test %>%
    mutate(LPC.pred = predict(rf2.LPC, newdata = .), LPC.error = LPC.pred - LPC,
           CPC.pred = predict(rf2.CPC, newdata = .), CPC.error = CPC.pred - CPC,
           NDP.pred = predict(rf2.NDP, newdata = .), NDP.error = NDP.pred - NDP,
           Green.pred = predict(rf2.Green, newdata = .), Green.error = Green.pred - Green,
           Bloc.pred = predict(rf2.Bloc, newdata = .), Bloc.error = Bloc.pred - Bloc)
  
  LPC_error[i] <- test$LPC.error[1]
  CPC_error[i] <- test$CPC.error[1]
  NDP_error[i] <- test$NDP.error[1]
  Green_error[i] <- test$Green.error[1]
  Bloc_error[i] <- test$Bloc.error[1]
}

rf2_demographics.errors <- results %>%
  mutate(LPC_error = LPC_error,
         CPC_error = CPC_error,
         NDP_error = NDP_error,
         Green_error = Green_error,
         Bloc_error = Bloc_error) %>%
  dplyr::select(district_code, name_english, year, incumbent, province, region, LPC_error, CPC_error, NDP_error, Green_error, Bloc_error)

write.csv(rf2_demographics.errors, file = "Output/Model testing/rf2_demographics_errors.csv", row.names = FALSE) 

## Error summary stats
# RMSE by party
rf2_demographics.errors %>%
  summarise(n = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, region
rf2_demographics.errors %>%
  group_by(region) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, year
rf2_demographics.errors %>%
  group_by(year) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

## Density plots 
# Party, overall
rf2_demographics.errors %>%
  filter(region != "The frigid northlands") %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 2",
       x = "Error", y = "Density")

# Party, by region
rf2_demographics.errors %>%
  filter(region != "The frigid northlands") %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 2",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/rf2_demographics_errors_region.png", width = 20, height = 12)

# Party, by year
rf2_demographics.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~year) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 2",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/rf2_demographics_errors_year.png", width = 20, height = 7)

#### LOOCV after feature pruning and mtry tuning ####
results <- historical_results.district %>%
  filter(year != 2004) %>%
  mutate_at(vars(contains("funds")), function(x) {
    x[is.na(x)] <- 0
    return(x)
  }) %>%
  mutate(Quebec = (province == "Quebec"),
         Atlantic = (region == "Atlantic"))

results$Bloc[is.na(results$Bloc)] <- 0
results$Bloc[is.na(results$Bloc)] <- 0

n <- nrow(results)
LPC_error <- CPC_error <- NDP_error <- Green_error <- Bloc_error <- rep(NA, n)

set.seed(2019)
for(i in 1:n) {
  cat("District ", results$district_code[i], ": ", results$name_english[i], " (", results$year[i], ")", "\n", sep = "")
  # Train/test split
  train <- results[-i,]
  test <- results[i,]
  
  # Fit random forest model (after tuning)
  rf_simple.LPC <- randomForest(LPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+LPC_nation+LPC_region+LPC_nation_lag+LPC_region_lag+
                                  educ_university+minority, 
                                data = results, mtry = 3, localImp = TRUE)
  rf_simple.CPC <- randomForest(CPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+CPC_nation+CPC_region+CPC_nation_lag+CPC_region_lag+minority, 
                                data = results, mtry = 3, localImp = TRUE)
  rf_simple.NDP <- randomForest(NDP~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+NDP_nation+NDP_region+NDP_nation_lag+NDP_region_lag+
                                  pop_growth_rate, data = results, mtry = 5, localImp = TRUE)
  rf_simple.Green <- randomForest(Green~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Green_nation+Green_region+Green_nation_lag+Green_region_lag+
                                    sex_female+minority, 
                                  data = results, mtry = 5, localImp = TRUE)
  rf_simple.Bloc <- randomForest(Bloc~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Bloc_nation+Bloc_region+Bloc_nation_lag+Bloc_region_lag+
                                   pop_growth_rate+educ_university, 
                                 data = results, mtry = 5, localImp = TRUE)
  
  # Make test prediction and compute error
  test <- test %>%
    mutate(LPC.pred = predict(rf_simple.LPC, newdata = .), LPC.error = LPC.pred - LPC,
           CPC.pred = predict(rf_simple.CPC, newdata = .), CPC.error = CPC.pred - CPC,
           NDP.pred = predict(rf_simple.NDP, newdata = .), NDP.error = NDP.pred - NDP,
           Green.pred = predict(rf_simple.Green, newdata = .), Green.error = Green.pred - Green,
           Bloc.pred = predict(rf_simple.Bloc, newdata = .), Bloc.error = Bloc.pred - Bloc)
  
  LPC_error[i] <- test$LPC.error[1]
  CPC_error[i] <- test$CPC.error[1]
  NDP_error[i] <- test$NDP.error[1]
  Green_error[i] <- test$Green.error[1]
  Bloc_error[i] <- test$Bloc.error[1]
}

rf_simple.errors <- results %>%
  mutate(LPC_error = LPC_error,
         CPC_error = CPC_error,
         NDP_error = NDP_error,
         Green_error = Green_error,
         Bloc_error = Bloc_error) %>%
  dplyr::select(district_code, name_english, year, incumbent, province, region, LPC_error, CPC_error, NDP_error, Green_error, Bloc_error)

write.csv(rf_simple.errors, file = "Output/Model testing/rf_simple_errors.csv", row.names = FALSE) 

## Error summary stats
# RMSE by party
rf_simple.errors %>%
  summarise(n = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, region
rf_simple.errors %>%
  group_by(region) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, year
rf_simple.errors %>%
  group_by(year) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

## Density plots 
# Party, overall
rf_simple.errors %>%
  filter(region != "The frigid northlands") %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 3",
       x = "Error", y = "Density")

# Party, by region
rf_simple.errors %>%
  filter(region != "The frigid northlands") %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 3",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/rf_simple_errors_region.png", width = 20, height = 12)

# Party, by year
rf_simple.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~year) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Random forest 3",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/rf_simple_errors_year.png", width = 20, height = 7)