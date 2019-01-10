#### Testing random forest models ####
source("Code/historical_results.R")
set.seed(2019)

## LOOCV on 2006
n <- nrow(results_2006)

error_2006 <- matrix(NA, n, 4)
colnames(error_2006) <- c("LPC", "CPC", "NDP", "Green")
rownames(error_2006) <- results_2006$district_code
for(i in 1:n) {
  # Train/test split
  train_2006 <- results_2006[-i,]
  test_2006 <- results_2006[i,]
  
  # Fit random forest model (default options)
  rf_2006.LPC <- randomForest(I(LPC-LPC_nation)~incumbent*(LPC_lag+CPC_lag+NDP_lag+Bloc_lag+LPC_region+LPC_nation_lag+LPC_region_lag),
                              data = train_2006)
  rf_2006.CPC <- randomForest(I(CPC-CPC_nation)~incumbent*(LPC_lag+CPC_lag+NDP_lag+Bloc_lag+LPC_region+LPC_nation+LPC_nation_lag+LPC_region_lag), 
                              data = train_2006)
  rf_2006.NDP <- randomForest(I(NDP-NDP_nation)~incumbent*(LPC_lag+CPC_lag+NDP_lag+Bloc_lag+LPC_region+LPC_nation+LPC_nation_lag+LPC_region_lag), 
                              data = train_2006)
  rf_2006.Green <- randomForest(I(Green-Green_nation)~incumbent*(LPC_lag+CPC_lag+NDP_lag+Bloc_lag+LPC_region+LPC_nation+LPC_nation_lag+LPC_region_lag), 
                                data = train_2006)
  
  # Make test prediction and compute error
  test_2006 <- test_2006 %>%
    mutate(LPC.pred = predict(rf_2006.LPC, newdata = .) + LPC_nation, LPC.error = LPC.pred - LPC,
           CPC.pred = predict(rf_2006.CPC, newdata = .) + CPC_nation, CPC.error = CPC.pred - CPC,
           NDP.pred = predict(rf_2006.NDP, newdata = .) + NDP_nation, NDP.error = NDP.pred - NDP,
           Green.pred = predict(rf_2006.Green, newdata = .) + Green_nation, Green.error = Green.pred - Green)
  
  error_2006[i,1] <- test_2006$LPC.error[1]
  error_2006[i,2] <- test_2006$CPC.error[1]
  error_2006[i,3] <- test_2006$NDP.error[1]
  error_2006[i,4] <- test_2006$Green.error[1]
}

## LOOCV on all years ##
error_allyears <- matrix(NA, n, 4)
colnames(error_2006) <- c("LPC", "CPC", "NDP", "Green")
rownames(error_2006) <- results_2006$district_code
for(i in 1:n) {
  # Train/test split
  train_2006 <- results_2006[-i,]
  test_2006 <- results_2006[i,]
  
  # Fit random forest model (default options)
  rf_2006.LPC <- randomForest(I(LPC-LPC_nation)~incumbent*(LPC_lag+CPC_lag+NDP_lag+Bloc_lag+LPC_region+LPC_nation_lag+LPC_region_lag),
                              data = train_2006)
  rf_2006.CPC <- randomForest(I(CPC-CPC_nation)~incumbent*(LPC_lag+CPC_lag+NDP_lag+Bloc_lag+LPC_region+LPC_nation+LPC_nation_lag+LPC_region_lag), 
                              data = train_2006)
  rf_2006.NDP <- randomForest(I(NDP-NDP_nation)~incumbent*(LPC_lag+CPC_lag+NDP_lag+Bloc_lag+LPC_region+LPC_nation+LPC_nation_lag+LPC_region_lag), 
                              data = train_2006)
  rf_2006.Green <- randomForest(I(Green-Green_nation)~incumbent*(LPC_lag+CPC_lag+NDP_lag+Bloc_lag+LPC_region+LPC_nation+LPC_nation_lag+LPC_region_lag), 
                                data = train_2006)
  
  # Make test prediction and compute error
  test_2006 <- test_2006 %>%
    mutate(LPC.pred = predict(rf_2006.LPC, newdata = .) + LPC_nation, LPC.error = LPC.pred - LPC,
           CPC.pred = predict(rf_2006.CPC, newdata = .) + CPC_nation, CPC.error = CPC.pred - CPC,
           NDP.pred = predict(rf_2006.NDP, newdata = .) + NDP_nation, NDP.error = NDP.pred - NDP,
           Green.pred = predict(rf_2006.Green, newdata = .) + Green_nation, Green.error = Green.pred - Green)
  
  error_2006[i,1] <- test_2006$LPC.error[1]
  error_2006[i,2] <- test_2006$CPC.error[1]
  error_2006[i,3] <- test_2006$NDP.error[1]
  error_2006[i,4] <- test_2006$Green.error[1]
}