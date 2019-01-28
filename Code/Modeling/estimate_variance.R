#### This requires that you've estimated the errors from the "random_forest_testing.R" script
source("Code/library.R")

## LOOCV errors from random forest model with population growth
rf_pop.errors <- fread_to_tbl("Output/Model testing/rf_pop_errors.csv")

mlm.LPC_error <- lmer(LPC_error~(1|province), data = rf_pop.errors, REML = FALSE)
mlm.CPC_error <- lmer(CPC_error~(1|province), data = rf_pop.errors, REML = FALSE)
mlm.NDP_error <- lmer(NDP_error~(1|province), data = rf_pop.errors, REML = FALSE)
mlm.Green_error <- lmer(Green_error~(1|province), data = rf_pop.errors, REML = FALSE)
mlm.Bloc_error <- lmer(Bloc_error~(1|province), data = rf_pop.errors, REML = FALSE)
