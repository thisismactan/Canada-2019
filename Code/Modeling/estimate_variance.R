#### This requires that you've estimated the errors from the "random_forest_testing.R" script
source("Code/library.R")

## LOOCV errors from random forest model with population growth
rf_pop.errors <- fread_to_tbl("Output/Model testing/rf_pop_errors.csv")

## Standard deviation (even by province means are essentially zero)
rf_pop.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region")) %>%
  group_by(region, variable) %>%
  summarise(error_sd = sd(value)) %>%
  spread(variable, error_sd)

## Error distributions
rf_pop.errors %>%
  ggplot(aes(x = LPC_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "red", position = "identity", binwidth = 0.02) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Liberal Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf_pop.errors %>%
  ggplot(aes(x = CPC_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "blue", position = "identity", binwidth = 0.02) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Conservative Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf_pop.errors %>%
  ggplot(aes(x = NDP_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "darkorange1", position = "identity", binwidth = 0.02) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "NDP leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

