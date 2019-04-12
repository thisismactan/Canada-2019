## Run necessaries
source("Code/poll_average.R")
source("Code/Modeling/poll_error_variance.R")
source("Code/Modeling/model_error_variance.R")
source("Code/shape_2019_data.R")
source("Code/simple_models.R")

## 
LPC_preds <- predict(model_LPC.simple, newdata = data_2019.simple)
CPC_preds <- predict(model_CPC.simple, newdata = data_2019.simple)
NDP_preds <- predict(model_NDP.simple, newdata = data_2019.simple)
Bloc_preds <- predict(model_Bloc.simple, newdata = data_2019.simple)
Green_preds <- predict(model_Green.simple, newdata = data_2019.simple)

predictions <- 

## Total covariance matrices (one for each province)