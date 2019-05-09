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
model_LPC.linear <- lm(LPC~incumbent_LPC+incumbent_NDP+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+Quebec+I(LPC_nation-LPC_nation_lag)+
                         I(LPC_region-LPC_region_lag)+educ_university+minority, data = results)
model_CPC.linear <- lm(CPC~incumbent_LPC+incumbent_CPC+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+CPC_nation+I(CPC_region-CPC_region_lag)+minority, 
                       data = results)
model_NDP.linear <- lm(NDP~incumbent_LPC+incumbent_CPC+incumbent_NDP+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+I(NDP_nation-NDP_nation_lag)+
                         I(NDP_region-NDP_region_lag)+age_65, data = results)
model_Green.linear <- lm(I(Green-Green_lag)~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+incumbent_Green+LPC_lag+CPC_lag+NDP_lag+Bloc_lag+
                           I(Green_nation-Green_nation_lag)+I(Green_region-Green_region_lag)+I(Green_lag < 0.05)+Vancouver_Island, data = results)
model_Bloc.linear <- lm(Bloc~incumbent_LPC+incumbent_CPC+incumbent_NDP+incumbent_Bloc+LPC_lag+CPC_lag+NDP_lag+Green_lag+Bloc_lag+
                          I(Bloc_nation-Bloc_nation_lag)+I(Bloc_region-Bloc_region_lag), data = results %>% filter(Quebec))

write_rds(list("LPC" = model_LPC.linear,
               "CPC" = model_CPC.linear,
               "NDP" = model_NDP.linear,
               "Green" = model_Green.linear,
               "Bloc" = model_Bloc.linear), "Shiny-app/models.rds")
