#### This requires that you've estimated the errors from the "random_forest_testing.R" script
source("Code/library.R")

## LOOCV errors from random forest model with population growth
rf_demographics.errors <- fread_to_tbl("Output/Model testing/rf_demographics_errors.csv")

## Standard deviation (even by province means are essentially zero)
rf_demographics.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region")) %>%
  group_by(region, variable) %>%
  summarise(error_sd = sd(value)) %>%
  spread(variable, error_sd)

## Error distributions - plots
rf_demographics.errors %>%
  ggplot(aes(x = LPC_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "red", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Liberal Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf_demographics.errors %>%
  ggplot(aes(x = CPC_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "blue", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Conservative Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf_demographics.errors %>%
  ggplot(aes(x = NDP_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "darkorange1", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "NDP leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf_demographics.errors %>%
  ggplot(aes(x = Green_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "green4", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Green Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf_demographics.errors %>%
  ggplot(aes(x = Bloc_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "#8ECEF9", position = "identity", binwidth = 0.02) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Bloc Québécois leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

## Error covariance by region
covariance_list <- vector("list", n_distinct(rf_pop.errors$region))
regions <- unique(rf_pop.errors$region)

for(i in 1:length(covariance_list)) {
  covariance_list[[i]] <- rf_pop.errors %>%
    filter(region == regions[i]) %>%
    dplyr::select(ends_with("error")) %>%
    cov()
}

names(covariance_list) <- regions