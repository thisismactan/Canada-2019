source("Code/historical_results.R")

#### Testing models ####
r.squareds <- matrix(NA, 4, 5)
rownames(r.squareds) <- c("Linear", "Linear + inc", "Logit", "Logit + inc")
colnames(r.squareds) <- c("LPC", "CPC", "NDP", "Bloc", "Green")

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

## Compute district elasticities (or swinginess) ##
district_elasticity <- historical_results.district %>%
  # Compute national (or regional for Bloc) changes
  mutate(LPC_nation_change = LPC_nation - lag(LPC_nation),
         CPC_nation_change = CPC_nation - lag(CPC_nation),
         NDP_nation_change = NDP_nation - lag(NDP_nation),
         Green_nation_change = Green_nation - lag(Green_nation),
         Bloc_region_change = Bloc_region - lag(Bloc_region)) %>%
  mutate(lag_incumbent = lag(incumbent),
         incumbent_run = incumbent == lag_incumbent,
         LPC_elast = LPC_change/LPC_nation_change,
         CPC_elast = CPC_change/CPC_nation_change,
         NDP_elast = NDP_change/NDP_nation_change,
         Green_elast = Green_change/Green_nation_change,
         Bloc_elast = Bloc_change/Bloc_region_change) %>%
  group_by(district_code) %>%
  summarise(LPC_elast_avg = mean(LPC_elast, na.rm = TRUE),
            CPC_elast_avg = mean(CPC_elast, na.rm = TRUE),
            NDP_elast_avg = mean(NDP_elast, na.rm = TRUE),
            Green_elast_avg = mean(Green_elast, na.rm = TRUE),
            Bloc_elast_avg = mean(Bloc_elast, na.rm = TRUE)) %>%
  ungroup()

historical_results.district_elast <- historical_results.district %>%
  left_join(district_elasticity, by = "district_code")

r.squareds <- matrix(NA, 2, 5)
rownames(r.squareds) <- c("Linear", "Logit")
colnames(r.squareds) <- c("LPC", "CPC", "NDP", "Bloc", "Green")

#### Test RMSE ####
results_2006 <- historical_results.district %>% filter(year == 2006)
results_2008 <- historical_results.district %>% filter(year == 2008)
results_2011 <- historical_results.district %>% filter(year == 2011)

lm.LPC_2006 <- lm(I(LPC-LPC_nation)~incumbent*I(province == "Quebec")*(LPC_region+LPC_lag), data = results_2006)
lm.LPC_2008 <- lm(I(LPC-LPC_nation)~incumbent*I(province == "Quebec")*(LPC_region+LPC_lag), data = results_2008)

results_2008.pred <- results_2008 %>%
  ungroup() %>%
  mutate(LPC_pred = predict(lm.LPC_2006, newdata = .) + LPC_nation,
         LPC_error = LPC_pred - LPC)

results_2011.pred <- results_2011 %>%
  ungroup() %>%
  mutate(LPC_pred = predict(lm.LPC_2008, newdata = .) + LPC_nation,
         LPC_error = LPC_pred - LPC)

results_2008.pred %>%
  ggplot(aes(x = LPC, y = LPC_error)) +
  geom_text(aes(label = district_code, col = incumbent), alpha = 0.5) +
  geom_smooth(col = "black") +
  scale_colour_manual(name = "Incumbent", values = c("#8ECEF9", "blue", "red", "darkorange1", "black"))

results_2011.pred %>%
  ggplot(aes(x = LPC, y = LPC_error)) +
  geom_text(aes(label = district_code, col = incumbent), alpha = 0.5) +
  geom_smooth(col = "black") +
  scale_colour_manual(name = "Incumbent", values = c("#8ECEF9", "blue", "red", "darkorange1", "black"))
