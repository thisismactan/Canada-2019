source("Code/historical_results.R")

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

## The official models to be run before any fundraising data comes in
model_LPC.simple <- randomForest(LPC~LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+LPC_nation+LPC_region+LPC_nation_lag+LPC_region_lag+
                                   educ_university+minority, 
                                 data = results, mtry = 3, localImp = TRUE)
model_CPC.simple <- randomForest(CPC~LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+CPC_nation+CPC_region+CPC_nation_lag+CPC_region_lag+minority, 
                                 data = results, mtry = 3, localImp = TRUE)
model_NDP.simple <- randomForest(NDP~LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+NDP_nation+NDP_region+NDP_nation_lag+NDP_region_lag+
                                   pop_growth_rate, 
                                 data = results, mtry = 5, localImp = TRUE)
model_Green.simple <- randomForest(Green~LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Green_nation+Green_region+Green_nation_lag+Green_region_lag+
                                     sex_female+minority, 
                                   data = results, mtry = 5, localImp = TRUE)
model_Bloc.simple <- randomForest(Bloc~LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Bloc_nation+Bloc_region+Bloc_nation_lag+Bloc_region_lag+
                                    Quebec+pop_growth_rate+educ_university, 
                                  data = results, mtry = 5, localImp = TRUE)

## Linear regression models for comparison
model_LPC.linear <- lm(LPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+LPC_nation+LPC_region+LPC_region_lag+
                         educ_university+minority, data = results)
model_CPC.linear <- lm(CPC~LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+CPC_nation+CPC_region+CPC_nation_lag+CPC_region_lag+minority, 
                       data = results)
model_NDP.linear <- lm(NDP~LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+NDP_nation+NDP_region+NDP_nation_lag+NDP_region_lag+pop_growth_rate, 
                       data = results)
model_Green.linear <- lm(Green~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Green_nation+Green_region+Green_nation_lag+Green_region_lag+sex_female+
                           minority, data = results)
model_Bloc.linear <- lm(Bloc~LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Bloc_nation+Bloc_region+Bloc_nation_lag+Bloc_region_lag+Quebec+
                          pop_growth_rate+educ_university, data = results)
