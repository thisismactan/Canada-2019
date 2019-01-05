#### Testing models ####
r.squareds <- matrix(NA, 4, 5)
row.names(r.squareds) <- c("Linear", "Linear + inc", "Logit", "Logit + inc")
col.names(r.squareds) <- c("LPC", "CPC", "NDP", "Bloc", "Green")

## Linear regression
# On linear scale
r.squareds[1,1] <- summary(lm(LPC~LPC_lag+LPC_region+LPC_nation, data = historical_results.district))$adj.r.squared
r.squareds[1,2] <- summary(lm(CPC~CPC_lag+CPC_region+CPC_nation, data = historical_results.district))$adj.r.squared
r.squareds[1,3] <- summary(lm(NDP~NDP_lag+NDP_region+NDP_nation, data = historical_results.district))$adj.r.squared
r.squareds[1,4] <- summary(lm(Bloc~Bloc_lag+Bloc_region+Bloc_nation, data = historical_results.district))$adj.r.squared
r.squareds[1,5] <- summary(lm(Green~Green_lag+Green_region+Green_nation, data = historical_results.district))$adj.r.squared

# On logit scale
r.squareds[3,1] <- summary(lm(LPC~LPC_lag+LPC_region+LPC_nation, data = historical_results.logit))$adj.r.squared
r.squareds[3,2] <- summary(lm(CPC~CPC_lag+CPC_region+CPC_nation, data = historical_results.logit))$adj.r.squared
r.squareds[3,3] <- summary(lm(NDP~NDP_lag+NDP_region+NDP_nation, data = historical_results.logit))$adj.r.squared
r.squareds[3,4] <- summary(lm(Bloc~Bloc_lag+Bloc_region+Bloc_nation, data = historical_results.logit))$adj.r.squared
r.squareds[3,5] <- summary(lm(Green~Green_lag+Green_region+Green_nation, data = historical_results.logit))$adj.r.squared

# Including incumbent interactions
r.squareds[2,1] <- summary(lm(LPC~incumbent*(LPC_lag+LPC_region+LPC_nation), data = historical_results.district))$adj.r.squared
r.squareds[2,2] <- summary(lm(CPC~incumbent*(CPC_lag+CPC_region+CPC_nation), data = historical_results.district))$adj.r.squared
r.squareds[2,3] <- summary(lm(NDP~incumbent*(NDP_lag+NDP_region+NDP_nation), data = historical_results.district))$adj.r.squared
r.squareds[2,4] <- summary(lm(Bloc~incumbent*(Bloc_lag+Bloc_region+Bloc_nation), data = historical_results.district))$adj.r.squared
r.squareds[2,5] <- summary(lm(Green~incumbent*(Green_lag+Green_region+Green_nation), data = historical_results.district))$adj.r.squared

r.squareds[4,1] <- summary(lm(LPC~incumbent*(LPC_lag+LPC_region+LPC_nation), data = historical_results.logit))$adj.r.squared
r.squareds[4,2] <- summary(lm(CPC~incumbent*(CPC_lag+CPC_region+CPC_nation), data = historical_results.logit))$adj.r.squared
r.squareds[4,3] <- summary(lm(NDP~incumbent*(NDP_lag+NDP_region+NDP_nation), data = historical_results.logit))$adj.r.squared
r.squareds[4,4] <- summary(lm(Bloc~incumbent*(Bloc_lag+Bloc_region+Bloc_nation), data = historical_results.logit))$adj.r.squared
r.squareds[4,5] <- summary(lm(Green~incumbent*(Green_lag+Green_region+Green_nation), data = historical_results.logit))$adj.r.squared

round(r.squareds, 3)