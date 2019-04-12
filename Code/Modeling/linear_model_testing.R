#### Testing linear regression models ####
source("Code/historical_results.R")

results <- historical_results.district %>%
  filter(year != 2004) %>%
  mutate_at(vars(contains("funds")), function(x) {
    x[is.na(x)] <- 0
    return(x)
  }) %>%
  mutate(Quebec = (province == "Quebec"),
         Atlantic = (region == "Atlantic"),
         incumbent_LPC = incumbent == "Liberal",
         incumbent_CPC = incumbent == "Conservative",
         incumbent_NDP = incumbent == "NDP",
         incumbent_Bloc = incumbent == "Bloc",
         incumbent_Green = incumbent == "Green")

results$Bloc[is.na(results$Bloc)] <- 0
results$Bloc[is.na(results$Bloc)] <- 0

n <- nrow(results)
LPC_error <- CPC_error <- NDP_error <- Green_error <- Bloc_error <- rep(NA, n)

for(i in 1:n) {
  cat("District ", results$district_code[i], ": ", results$name_english[i], " (", results$year[i], ")", "\n", sep = "")
  # Train/test split
  train <- results[-i,]
  test <- results[i,]
  
  # Fit linear models
  model_LPC.linear <- lm(LPC~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+LPC_nation+
                           LPC_region+LPC_region_lag+educ_university+minority, data = train)
  model_CPC.linear <- lm(CPC~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+CPC_nation+
                           CPC_region+CPC_nation_lag+CPC_region_lag+minority, data = train)
  model_NDP.linear <- lm(NDP~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+NDP_nation+
                           NDP_region+NDP_nation_lag+NDP_region_lag, data = train)
  model_Green.linear <- lm(Green~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Green_nation+
                             Green_region+Green_nation_lag+Green_region_lag+minority, data = train)
  model_Bloc.linear <- lm(Bloc~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Bloc_region+
                            Bloc_region_lag, data = train)
  
  # Make test prediction and compute error
  test <- test %>%
    mutate(LPC.pred = predict(model_LPC.linear, newdata = .), LPC.error = LPC.pred - LPC,
           CPC.pred = predict(model_CPC.linear, newdata = .), CPC.error = CPC.pred - CPC,
           NDP.pred = predict(model_NDP.linear, newdata = .), NDP.error = NDP.pred - NDP,
           Green.pred = predict(model_Green.linear, newdata = .), Green.error = Green.pred - Green,
           Bloc.pred = predict(model_Bloc.linear, newdata = .), Bloc.error = Bloc.pred - Bloc)
  
  LPC_error[i] <- test$LPC.error[1]
  CPC_error[i] <- test$CPC.error[1]
  NDP_error[i] <- test$NDP.error[1]
  Green_error[i] <- test$Green.error[1]
  Bloc_error[i] <- test$Bloc.error[1]
}

linear_model.errors <- results %>%
  mutate(LPC_error = LPC_error,
         CPC_error = CPC_error,
         NDP_error = NDP_error,
         Green_error = Green_error,
         Bloc_error = Bloc_error) %>%
  dplyr::select(district_code, name_english, year, incumbent, province, region, LPC_error, CPC_error, NDP_error, Green_error, Bloc_error)

write.csv(linear_model.errors, file = "Output/Model testing/linear_model_errors.csv", row.names = FALSE) 

## RMSE
linear_model.errors %>%
  summarise(n = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, region
linear_model.errors %>%
  group_by(region) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, year
linear_model.errors %>%
  group_by(year) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

## Density plots 
# Party, overall
linear_model.errors %>%
  filter(region != "The frigid northlands") %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Linear regression model",
       x = "Error", y = "Density")

# Party, by region
linear_model.errors %>%
  filter(region != "The frigid northlands") %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Linear regression model",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/linear_model_errors_region.png", width = 20, height = 12)

# Party, by year
linear_model.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~year) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Linear regression model",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/linear_model_errors_year.png", width = 20, height = 7)

