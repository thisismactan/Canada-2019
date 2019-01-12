#### Testing gradient boosted tree models ####
source("Code/historical_results.R")

#### LOOCV on all years with population predictors ####
results <- historical_results.district %>%
  filter(year != 2004) %>%
  mutate_at(vars(contains("funds")), function(x) {
    x[is.na(x)] <- 0
    return(x)
  }) %>%
  mutate(Quebec = (province == "Quebec"))

results$Bloc[is.na(results$Bloc)] <- 0
results$Bloc[is.na(results$Bloc)] <- 0

## Initializing error vectors
n <- nrow(results)
LPC_error <- CPC_error <- NDP_error <- Green_error <- Bloc_error <- rep(NA, n)

## Choosing features
LPC.x <- results %>%
  dplyr::select(incumbent, LPC_lag, CPC_lag, NDP_lag, Green_lag, Bloc_lag, Quebec, LPC_nation, LPC_region, LPC_nation_lag, LPC_region_lag,
                LPC_funds, LPC_funds_frac, pop, pop_growth_rate)
CPC.x <- results %>%
  dplyr::select(incumbent, LPC_lag, CPC_lag, NDP_lag, Green_lag, Bloc_lag, Quebec, CPC_nation, CPC_region, CPC_nation_lag, CPC_region_lag,
                CPC_funds, CPC_funds_frac, pop, pop_growth_rate)
NDP.x <- results %>%
  dplyr::select(incumbent, LPC_lag, CPC_lag, NDP_lag, Green_lag, Bloc_lag, Quebec, NDP_nation, NDP_region, NDP_nation_lag, NDP_region_lag,
                NDP_funds, NDP_funds_frac, pop, pop_growth_rate)
Green.x <- results %>%
  dplyr::select(incumbent, LPC_lag, CPC_lag, NDP_lag, Green_lag, Bloc_lag, Quebec, Green_nation, Green_region, Green_nation_lag, Green_region_lag,
                Green_funds, Green_funds_frac, pop, pop_growth_rate)
Bloc.x <- results %>%
  dplyr::select(incumbent, LPC_lag, CPC_lag, NDP_lag, Green_lag, Bloc_lag, Quebec, Bloc_nation, Bloc_region, Bloc_nation_lag, Bloc_region_lag,
                Bloc_funds, Bloc_funds_frac, pop, pop_growth_rate)

## Convert to sparse matrix
LPC.matrix <- model.matrix(~., LPC.x)
CPC.matrix <- model.matrix(~., CPC.x)
NDP.matrix <- model.matrix(~., NDP.x)
Green.matrix <- model.matrix(~., Green.x)
Bloc.matrix <- model.matrix(~., Bloc.x)

set.seed(2019)
for(i in 1:n) {
  cat(i, "\n")
  
  ## Subset training features (x) and labels (y)
  train_LPC.x <- LPC.matrix[-i,]
  train_CPC.x <- CPC.matrix[-i,]
  train_NDP.x <- NDP.matrix[-i,]
  train_Green.x <- Green.matrix[-i,]
  train_Bloc.x <- Bloc.matrix[-i,]
  
  train_LPC.y <- results$LPC[-i]
  train_CPC.y <- results$CPC[-i]
  train_NDP.y <- results$NDP[-i]
  train_Green.y <- results$Green[-i]
  train_Bloc.y <- results$Bloc[-i]
  
  ## Subset test features (x) and labels (y)
  test_LPC.x <- t(as.matrix(LPC.matrix[i,]))
  test_CPC.x <- t(as.matrix(CPC.matrix[i,]))
  test_NDP.x <- t(as.matrix(NDP.matrix[i,]))
  test_Green.x <- t(as.matrix(Green.matrix[i,]))
  test_Bloc.x <- t(as.matrix(Bloc.matrix[i,]))
  
  test_LPC.y <- results$LPC[i]
  test_CPC.y <- results$CPC[i]
  test_NDP.y <- results$NDP[i]
  test_Green.y <- results$Green[i]
  test_Bloc.y <- results$Bloc[i]
  
  ## Fit models
  xgb.LPC <- xgboost(data = train_LPC.x, label = train_LPC.y, nrounds = 300, eta = 0.2, max_depth = 2, early_stopping_rounds = 20,
                     verbose = 0)
  xgb.CPC <- xgboost(data = train_CPC.x, label = train_CPC.y, nrounds = 300, eta = 0.2, max_depth = 3, early_stopping_rounds = 20,
                     verbose = 0)
  xgb.NDP <- xgboost(data = train_NDP.x, label = train_NDP.y, nrounds = 300, eta = 0.2, max_depth = 3, early_stopping_rounds = 20,
                     verbose = 0)
  xgb.Green <- xgboost(data = train_Green.x, label = train_Green.y, nrounds = 300, eta = 0.2, max_depth = 3, early_stopping_rounds = 20,
                       verbose = 0)
  xgb.Bloc <- xgboost(data = train_Bloc.x, label = train_Green.y, nrounds = 300, eta = 0.2, max_depth = 3, early_stopping_rounds = 20,
                      verbose = 0)
  
  ## Predict on test observation
  LPC_pred <- predict(xgb.LPC, newdata = test_LPC.x)
  CPC_pred <- predict(xgb.CPC, newdata = test_CPC.x)
  NDP_pred <- predict(xgb.NDP, newdata = test_NDP.x)
  Green_pred <- predict(xgb.Green, newdata = test_Green.x)
  Bloc_pred <- predict(xgb.Bloc, newdata = test_Bloc.x)
  
  ## Compute error
  LPC_error[i] <- LPC_pred - test_LPC.y
  CPC_error[i] <- CPC_pred - test_CPC.y
  NDP_error[i] <- NDP_pred - test_NDP.y
  Green_error[i] <- Green_pred - test_Green.y
  Bloc_error[i] <- Bloc_pred - test_Bloc.y
}

xgb.errors <- results %>%
  dplyr::select(district_code, name_english, year, incumbent, province, region) %>%
  mutate(LPC = LPC_error,
         CPC = CPC_error,
         NDP = NDP_error,
         Green = Green_error,
         Bloc = Bloc_error)

write.csv(xgb.errors, file = "Output/Model testing/xgb_errors.csv", row.names = FALSE)

## Error summary stats
# RMSE by party
xgb.errors %>%
  summarise(n = n(),
            LPC = sqrt(mean_squares(LPC)),
            CPC = sqrt(mean_squares(CPC)),
            NDP = sqrt(mean_squares(NDP)),
            Green = sqrt(mean_squares(Green)),
            Bloc = sqrt(mean_squares(Bloc)))

# RMSE by party, region
xgb.errors %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC)),
            CPC = sqrt(mean_squares(CPC)),
            NDP = sqrt(mean_squares(NDP)),
            Green = sqrt(mean_squares(Green)),
            Bloc = sqrt(mean_squares(Bloc)))

# RMSE by party, year
xgb.errors %>%
  group_by(year) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC)),
            CPC = sqrt(mean_squares(CPC)),
            NDP = sqrt(mean_squares(NDP)),
            Green = sqrt(mean_squares(Green)),
            Bloc = sqrt(mean_squares(Bloc)))

## Density plots 
# Party, by region
xgb.errors %>%
  filter(region != "The frigid northlands") %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = c("red", "blue", "darkorange1", "green4", "#8ECEF9"), labels = c("LPC", "CPC", "NDP", "Green", "Bloc")) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Gradient boosting 1",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/xgb_errors_region.png", width = 16, height = 9)

# Party, by year
xgb.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~year) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = c("red", "blue", "darkorange1", "green4", "#8ECEF9"), labels = c("LPC", "CPC", "NDP", "Green", "Bloc")) +
  labs(title = "Distribution of errors by party and year",
       subtitle = "Random forest 2",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/xgb_errors_year.png", width = 16, height = 5)
