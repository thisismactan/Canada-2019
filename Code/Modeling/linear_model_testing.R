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
         Vancouver_Island = district_code %in% c(59008, 59014, 59015, 59024, 59031, 59035),
         incumbent = relevel(incumbent, ref = "None"),
         incumbent_LPC = incumbent == "Liberal",
         incumbent_CPC = incumbent == "Conservative",
         incumbent_NDP = incumbent == "NDP",
         incumbent_Green = incumbent == "Green",
         incumbent_Bloc = incumbent == "Bloc",
         incumbent_PPC = name_english == "Beauce")

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
  model_LPC.linear_test <- lm(LPC~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+LPC_nation+
                                LPC_region+LPC_region_lag+educ_university+minority, data = train)
  model_CPC.linear_test <- lm(CPC~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+CPC_nation+
                                CPC_region+CPC_nation_lag+CPC_region_lag+minority, data = train)
  model_NDP.linear_test <- lm(NDP~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+NDP_nation+
                                NDP_region+NDP_nation_lag+NDP_region_lag, data = train)
  model_Green.linear_test <- lm(Green~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Green_nation+
                                  Green_region+Green_nation_lag+Green_region_lag+minority, data = train)
  model_Bloc.linear_test <- lm(Bloc~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Bloc_region+
                                 Bloc_region_lag, data = train)
  
  # Make test prediction and compute error
  test <- test %>%
    mutate(LPC.pred = predict(model_LPC.linear_test, newdata = .), LPC.error = LPC.pred - LPC,
           CPC.pred = predict(model_CPC.linear_test, newdata = .), CPC.error = CPC.pred - CPC,
           NDP.pred = predict(model_NDP.linear_test, newdata = .), NDP.error = NDP.pred - NDP,
           Green.pred = predict(model_Green.linear_test, newdata = .), Green.error = Green.pred - Green,
           Bloc.pred = predict(model_Bloc.linear_test, newdata = .), Bloc.error = Bloc.pred - Bloc)
  
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

write.csv(linear_model.errors, file = "Output/Model testing/linear_model_errors2.csv", row.names = FALSE) 

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

#### Using regional and national swings instead of raw numbers ####
results <- historical_results.district %>%
  filter(year != 2004) %>%
  mutate_at(vars(contains("funds")), function(x) {
    x[is.na(x)] <- 0
    return(x)
  }) %>%
  mutate(Quebec = (province == "Quebec"),
         Atlantic = (region == "Atlantic"),
         Vancouver_Island = district_code %in% c(59008, 59014, 59015, 59024, 59031, 59035),
         incumbent = relevel(incumbent, ref = "None"),
         incumbent_LPC = incumbent == "Liberal",
         incumbent_CPC = incumbent == "Conservative",
         incumbent_NDP = incumbent == "NDP",
         incumbent_Green = incumbent == "Green",
         incumbent_Bloc = incumbent == "Bloc",
         incumbent_PPC = name_english == "Beauce")

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
  model_LPC.linear_test2 <- lm(LPC~incumbent_LPC+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+I(LPC_nation-LPC_nation_lag)+I(LPC_region-LPC_region_lag)+
                                educ_university+minority, data = train)
  model_CPC.linear_test2 <- lm(CPC~incumbent_LPC+incumbent_CPC+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+CPC_nation+I(CPC_region-CPC_region_lag)+minority, 
                               data = train)
  model_NDP.linear_test2 <- lm(NDP~incumbent_LPC+incumbent_CPC+incumbent_NDP+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+
                                 I(NDP_nation-NDP_nation_lag)+I(NDP_region-NDP_region_lag)+age_65, data = train)
  model_Green.linear_test2 <- lm(I(Green-Green_lag)~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+incumbent_Green+LPC_lag+CPC_lag+NDP_lag+
                                   Bloc_lag+I(Green_nation-Green_nation_lag)+I(Green_region-Green_region_lag)+Vancouver_Island, 
                                 data = train)
  model_Bloc.linear_test2 <- lm(Bloc~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+
                                  I(Bloc_nation-Bloc_nation_lag)+I(Bloc_region-Bloc_region_lag), data = train %>% filter(Quebec))
  
  # Make test prediction and compute error
  test <- test %>%
    mutate(LPC.pred = predict(model_LPC.linear_test2, newdata = .), LPC.error = LPC.pred - LPC,
           CPC.pred = predict(model_CPC.linear_test2, newdata = .), CPC.error = CPC.pred - CPC,
           NDP.pred = predict(model_NDP.linear_test2, newdata = .), NDP.error = NDP.pred - NDP,
           Green.pred = predict(model_Green.linear_test2, newdata = .) + Green_lag, Green.error = Green.pred - Green,
           Bloc.pred = case_when(Quebec ~ predict(model_Bloc.linear_test2, newdata = .),
                                 !Quebec ~ 0), 
           Bloc.error = Bloc.pred - Bloc)
  
  LPC_error[i] <- test$LPC.error[1]
  CPC_error[i] <- test$CPC.error[1]
  NDP_error[i] <- test$NDP.error[1]
  Green_error[i] <- test$Green.error[1]
  Bloc_error[i] <- test$Bloc.error[1]
}

linear_model.errors2 <- results %>%
  mutate(LPC_error = LPC_error,
         CPC_error = CPC_error,
         NDP_error = NDP_error,
         Green_error = Green_error,
         Bloc_error = Bloc_error) %>%
  dplyr::select(district_code, name_english, year, incumbent, province, region, LPC_error, CPC_error, NDP_error, Green_error, Bloc_error)

write.csv(linear_model.errors2, file = "Output/Model testing/linear_model_errors2.csv", row.names = FALSE) 

## RMSE
linear_model.errors2 %>%
  summarise(n = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, region
linear_model.errors2 %>%
  group_by(region) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

# RMSE by party, year
linear_model.errors2 %>%
  group_by(year) %>%
  summarise(districts = n(),
            LPC = sqrt(mean_squares(LPC_error)),
            CPC = sqrt(mean_squares(CPC_error)),
            NDP = sqrt(mean_squares(NDP_error)),
            Green = sqrt(mean_squares(Green_error)),
            Bloc = sqrt(mean_squares(Bloc_error)))

## Density plots 
# Party, overall
linear_model.errors2 %>%
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
linear_model.errors2 %>%
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

ggsave(filename = "Output/Model graphs/linear_model_errors_region2.png", width = 20, height = 12)

# Party, by year
linear_model.errors2 %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"),
       variable.name = "party", value.name = "error") %>%
  ggplot(aes(x = error, fill = party)) +
  facet_wrap(~year) +
  geom_histogram(col = "black", binwidth = 0.02) +
  scale_fill_manual(name = "Party", values = quebec_colors[1:5], labels = quebec_parties[1:5]) +
  labs(title = "Distribution of errors by party and region",
       subtitle = "Linear regression model",
       x = "Error", y = "Observations")

ggsave(filename = "Output/Model graphs/linear_model_errors_year2.png", width = 20, height = 7)

## Use AIC and BIC for model selection

#### Liberals ####
model_LPC.initial <- lm(LPC~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+LPC_nation+
                          LPC_region+LPC_nation_lag+LPC_region_lag+CPC_nation+CPC_region+sex_female+age_65+educ_university+minority+
                          pop_growth_rate, 
                        data = results)

## AIC
LPC.stepAIC <- step(model_LPC.initial, scope = ~., direction = "both")

## BIC
LPC.stepBIC <- step(model_LPC.initial, scope = ~., direction = "both", k = log(n))

#### Conservatives ####
model_CPC.initial <- lm(CPC~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+CPC_nation+
                          CPC_region+CPC_nation_lag+CPC_region_lag+LPC_nation+LPC_region+LPC_region_lag+sex_female+age_65+educ_university+minority+
                          pop_growth_rate, 
                        data = results)

## AIC
CPC.stepAIC <- step(model_CPC.initial, scope = ~., direction = "both")

## BIC
CPC.stepBIC <- step(model_CPC.initial, scope = ~., direction = "both", k = log(n))

#### NDP ####
model_NDP.initial <- lm(NDP~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+NDP_nation+
                          NDP_region+NDP_lag+NDP_nation_lag+NDP_region+LPC_nation_lag+LPC_region_lag+CPC_nation+CPC_region+sex_female+age_65+
                          educ_university+minority+pop_growth_rate,  
                        data = results)

## AIC
NDP.stepAIC <- step(model_NDP.initial, scope = ~., direction = "both")

## BIC
NDP.stepBIC <- step(model_NDP.initial, scope = ~., direction = "both", k = log(n))

#### Green ####
model_Green.initial <- lm(Green~incumbent_Green+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+Green_nation+Green_region+Green_lag+Green_nation_lag+
                            Green_region_lag+LPC_nation+LPC_region+CPC_nation+CPC_region+sex_female+age_65+educ_university+minority+pop_growth_rate,  
                          data = results)

## AIC
Green.stepAIC <- step(model_Green.initial, scope = ~., direction = "both")

## BIC
Green.stepBIC <- step(model_Green.initial, scope = ~., direction = "both", k = log(n))

#### Bloc ####
model_Bloc.initial <- lm(Bloc~Quebec*(incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Bloc_nation+
                           Bloc_region+Bloc_lag+Bloc_nation_lag+Bloc_region+LPC_nation_lag+LPC_region_lag+CPC_nation+CPC_region+sex_female+age_65+
                           educ_university+minority+pop_growth_rate), 
                         data = results)

## AIC
Bloc.stepAIC <- step(model_Bloc.initial, scope = ~., direction = "both")

## BIC
Bloc.stepBIC <- step(model_Bloc.initial, scope = ~., direction = "both", k = log(n))

#### Selected models ####
n <- nrow(results)
LPC_error.AIC <- LPC_error.BIC <- CPC_error.AIC <- CPC_error.BIC <- NDP_error.AIC <- Green_error.AIC <- Green_error.BIC <- Bloc_error.AIC <- 
  Bloc_error.BIC <- rep(NA, n)

for(i in 1:n) {
  cat("District ", results$district_code[i], ": ", results$name_english[i], " (", results$year[i], ")\n", sep = "")
  ## Train/test split
  train <- results[-i,]
  test <- results[i,]
  
  ## Models (AIC and BIC agree on the NDP model)
  LPC_model_cv.AIC <- lm(LPC~incumbent_LPC+incumbent_NDP+LPC_lag+CPC_lag+NDP_lag+Bloc_lag+LPC_nation+LPC_nation_lag+LPC_region+LPC_region_lag+
                           educ_university+minority+pop_growth_rate, data = train)
  
  LPC_model_cv.BIC <- lm(LPC~incumbent_LPC+LPC_lag+CPC_lag+NDP_lag+Bloc_lag+LPC_nation+LPC_nation_lag+LPC_region+LPC_region_lag+minority, 
                         data = train)
  
  CPC_model_cv.AIC <- lm(CPC~incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+CPC_lag+CPC_region+CPC_region_lag+
                           CPC_nation+CPC_nation_lag+Quebec+educ_university, data = train)
  
  CPC_model_cv.BIC <- lm(CPC~incumbent_CPC+LPC_lag+CPC_lag+NDP_lag+Bloc_lag+CPC_nation+CPC_nation_lag+CPC_region+CPC_region_lag+Quebec+educ_university,
                         data = train)
  
  NDP_model_cv.AIC <- lm(NDP~incumbent_LPC+incumbent_CPC+incumbent_NDP+NDP_lag+Quebec+NDP_nation+NDP_region+NDP_nation_lag+NDP_region_lag+
                           LPC_region_lag+CPC_region+age_65, data = train)
  
  Green_model_cv.AIC <- lm(Green~incumbent_Green+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Green_region+Green_region_lag+Green_nation_lag+
                             age_65+educ_university+minority, data = train)
  
  Green_model_cv.BIC <- lm(Green~Green_lag+Green_region+Green_region_lag+educ_university+minority, data = train)
  
  Bloc_model_cv.AIC <- lm(Bloc~Quebec+Quebec:(incumbent_LPC+incumbent_CPC+incumbent_NDP+LPC_lag+CPC_lag+NDP_lag+Bloc_nation+educ_university+age_65)+
                            Bloc_lag+Bloc_region, data = train)
  
  Bloc_model_cv.BIC <- lm(Bloc~Quebec+Quebec:(NDP_lag+Bloc_nation+educ_university)+Bloc_lag+Bloc_region, data = train)
  
  ## LOOCV
  LPC_error.AIC[i] <- predict(LPC_model_cv.AIC, data = test) - test$LPC
  LPC_error.BIC[i] <- predict(LPC_model_cv.BIC, data = test) - test$LPC
  CPC_error.AIC[i] <- predict(CPC_model_cv.AIC, data = test) - test$CPC
  CPC_error.BIC[i] <- predict(CPC_model_cv.BIC, data = test) - test$CPC
  NDP_error.AIC[i] <- predict(NDP_model_cv.AIC, data = test) - test$NDP
  Green_error.AIC[i] <- predict(Green_model_cv.AIC, data = test) - test$Green
  Green_error.BIC[i] <- predict(Green_model_cv.BIC, data = test) - test$Green
  Bloc_error.AIC[i] <- predict(Bloc_model_cv.AIC, data = test) - test$Bloc
  Bloc_error.BIC[i] <- predict(Bloc_model_cv.BIC, data = test) - test$Bloc
}

#### RMSE ####
## Liberal
sqrt(mean(LPC_error.AIC^2))
sqrt(mean(LPC_error.BIC^2))

## Conservative
sqrt(mean(CPC_error.AIC^2))
sqrt(mean(CPC_error.BIC^2))

## NDP
sqrt(mean(NDP_error.AIC^2))

## Green
sqrt(mean(Green_error.AIC^2))
sqrt(mean(Green_error.BIC^2))

## Bloc
sqrt(mean(Bloc_error.AIC^2))
sqrt(mean(Bloc_error.BIC^2))

## THIS IS A DISASTER AND I HAVE NO IDEA WHY AHHHHHH
