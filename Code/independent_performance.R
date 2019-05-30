source("Code/library.R")

independent_results <- read_csv("Data/independent_results.csv") %>%
  mutate(fraction = pct/(pct + former_party_pct),
         logit = logit(fraction))

independent.lm <- lm(fraction~last_pct+scandal, data = independent_results)
independent.logit <- lm(logit~last_pct+scandal, data = independent_results)

summary(independent.lm)
summary(independent.logit)

## Individual expected fractions
wilson_raybould_mean.logit <- predict(independent.logit, 
                                newdata = data.frame(last_pct = 0.4393, scandal = 0)) 

philpott_mean.logit <- predict(independent.logit,
                         newdata = data.frame(last_pct = 0.4921, scandal = 0)) 

grewal_mean.logit <- predict(independent.logit,
                             newdata = data.frame(last_pct = 0.5232, scandal = 1))

weir_mean.logit <- predict(independent.logit,
                           newdata = data.frame(last_pct = 0.3521, scandal = 1))

tootoo_mean.logit <- predict(independent.logit,
                             newdata = data.frame(last_pct = 0.4711, scandal = 0))

independent_se <- summary(independent.logit)$sigma
