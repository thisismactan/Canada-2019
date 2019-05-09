#### This requires that you've estimated the errors from the "random_forest_testing.R" script
source("Code/library.R")

#### Full model (pruned) ####
linear_model.errors <- fread_to_tbl("Output/Model testing/linear_model_errors2.csv")

## Standard deviation (even by province means are essentially zero)
linear_model.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region")) %>%
  group_by(region, variable) %>%
  summarise(error_sd = sd(value)) %>%
  spread(variable, error_sd)

## Error distributions - plots
linear_model.errors %>%
  ggplot(aes(x = LPC_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "red", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Liberal Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

linear_model.errors %>%
  ggplot(aes(x = CPC_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "blue", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Conservative Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

linear_model.errors %>%
  ggplot(aes(x = NDP_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "darkorange1", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "NDP leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

linear_model.errors %>%
  ggplot(aes(x = Green_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "green4", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Green Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

linear_model.errors %>%
  ggplot(aes(x = Bloc_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "#8ECEF9", position = "identity", binwidth = 0.02) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Bloc Québécois leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

## Checked linear mixed models on errors with random intercepts for region; we can ignore region effects
linear_model.error_covariance <- linear_model.errors %>%
  dplyr::select(LPC_error, CPC_error, NDP_error, Bloc_error, Green_error) %>%
  cov()
