## Testing out the randomForestExplainer package
source("Code/historical_results.R")

results <- historical_results.district %>%
  filter(year != 2004) %>%
  mutate_at(vars(contains("funds")), function(x) {
    x[is.na(x)] <- 0
    return(x)
  }) %>%
  mutate(Quebec = (province == "Quebec"))

#### Fitting models ####
set.seed(2019)
model_LPC <- randomForest(LPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+LPC_nation+LPC_region+LPC_nation_lag+LPC_region_lag+
                            LPC_funds+LPC_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, 
                          data = results, mtry = 9, localImp = TRUE)
model_CPC <- randomForest(CPC~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+CPC_nation+CPC_region+CPC_nation_lag+CPC_region_lag+
                            CPC_funds+CPC_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, 
                          data = results, mtry = 9, localImp = TRUE)
model_NDP <- randomForest(NDP~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+NDP_nation+NDP_region+NDP_nation_lag+NDP_region_lag+
                            NDP_funds+NDP_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, 
                          data = results, mtry = 9, localImp = TRUE)
model_Green <- randomForest(Green~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+Green_nation+Green_region+Green_nation_lag+
                              Green_region_lag+Green_funds+Green_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, 
                            data = results, mtry = 9, localImp = TRUE)
model_Bloc <- randomForest(Bloc~incumbent+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+Bloc_nation+Bloc_region+Bloc_nation_lag+Bloc_region_lag+
                             Bloc_funds+Bloc_funds_frac+pop_growth_rate+age_65+educ_university+sex_female+minority, 
                           data = results, mtry = 9, localImp = TRUE)

#### Create explainers ####
explain_forest(model_LPC)
explain_forest(model_CPC)
explain_forest(model_NDP)
explain_forest(model_Green)
explain_forest(model_Bloc)

#### Minimal depth distributions ####
LPC_min_depth_df <- min_depth_distribution(model_LPC)
CPC_min_depth_df <- min_depth_distribution(model_CPC)
NDP_min_depth_df <- min_depth_distribution(model_NDP)
Green_min_depth_df <- min_depth_distribution(model_Green)
Bloc_min_depth_df <- min_depth_distribution(model_Bloc)

plot_min_depth_distribution(LPC_min_depth_df, k = 18)
plot_min_depth_distribution(CPC_min_depth_df, k = 18)
plot_min_depth_distribution(NDP_min_depth_df, k = 18)
plot_min_depth_distribution(Bloc_min_depth_df, k = 18)
plot_min_depth_distribution(Green_min_depth_df, k = 18)