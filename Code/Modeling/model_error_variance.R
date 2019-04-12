#### This requires that you've estimated the errors from the "random_forest_testing.R" script
source("Code/library.R")

#### Full model (pruned) ####
rf2_demographics.errors <- fread_to_tbl("Output/Model testing/rf2_demographics_errors.csv")

## Standard deviation (even by province means are essentially zero)
rf2_demographics.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region")) %>%
  group_by(region, variable) %>%
  summarise(error_sd = sd(value)) %>%
  spread(variable, error_sd)

## Error distributions - plots
rf2_demographics.errors %>%
  ggplot(aes(x = LPC_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "red", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Liberal Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf2_demographics.errors %>%
  ggplot(aes(x = CPC_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "blue", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Conservative Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf2_demographics.errors %>%
  ggplot(aes(x = NDP_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "darkorange1", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "NDP leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf2_demographics.errors %>%
  ggplot(aes(x = Green_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "green4", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Green Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf2_demographics.errors %>%
  ggplot(aes(x = Bloc_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "#8ECEF9", position = "identity", binwidth = 0.02) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Bloc Québécois leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

## Error covariance by region
covariance_list <- vector("list", n_distinct(rf2_demographics.errors$region))
regions <- unique(rf_pop.errors$region)

for(i in 1:length(covariance_list)) {
  covariance_list[[i]] <- rf2_demographics.errors %>%
    filter(region == regions[i]) %>%
    dplyr::select(ends_with("error")) %>%
    cov()
}

names(covariance_list) <- regions

#### Simpler model without fundraising data ####
rf_simple.errors <- fread_to_tbl("Output/Model testing/rf_simple_errors.csv")

## Standard deviation (even by province means are essentially zero)
rf_simple.errors %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region")) %>%
  group_by(region, variable) %>%
  summarise(error_sd = sd(value)) %>%
  spread(variable, error_sd)

## Error distributions - plots
rf_simple.errors %>%
  ggplot(aes(x = LPC_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "red", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Liberal Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf_simple.errors %>%
  ggplot(aes(x = CPC_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "blue", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Conservative Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf_simple.errors %>%
  ggplot(aes(x = NDP_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "darkorange1", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "NDP leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf_simple.errors %>%
  ggplot(aes(x = Green_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "green4", position = "identity", binwidth = 0.01) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Green Party leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

rf_simple.errors %>%
  ggplot(aes(x = Bloc_error)) +
  facet_wrap(~region) +
  geom_histogram(col = "black", fill = "#8ECEF9", position = "identity", binwidth = 0.02) +
  lims(x = c(-0.4, 0.4)) +
  labs(title = "Bloc Québécois leave-one-out errors", x = "Error in popular vote",
       y = "District-years")

## Error covariance by region
covariance_list.simple <- vector("list", n_distinct(rf_simple.errors$region))
regions <- unique(rf_simple.errors$region)

for(i in 1:length(covariance_list)) {
  covariance_list.simple[[i]] <- rf_simple.errors %>%
    filter(region == regions[i]) %>%
    dplyr::select(ends_with("error")) %>%
    cov()
}

names(covariance_list.simple) <- regions

both_errors <- rf2_demographics.errors %>% 
  left_join(rf_simple.errors, by = c("district_code", "name_english", "year", "incumbent", "province", "region")) %>%
  melt(id.vars = c("district_code", "name_english", "year", "incumbent", "province", "region"), value.name = "pct",
       variable.name = "party") %>%
  as.tbl() %>%
  mutate(model = case_when(grepl("x", party) ~ "Full",
                           grepl("y", party) ~ "Simple"),
         party = gsub("_error[[:punct:]][xy]", "", party)) %>%
  spread(model, pct) %>%
  mutate(party = ordered(party, levels = c("LPC", "CPC", "NDP", "Bloc", "Green")))

ggplot(both_errors, aes(x = Full, y = Simple, col = party)) +
  facet_wrap(~party) +
  geom_point(alpha = 0.5) +
  scale_colour_manual(name = "Party", labels = c("Liberal", "Conservative", "NDP", "Bloc Québécois", "Green"), 
                      values = c("red", "blue", "darkorange1", "#8ECEF9", "green4")) +
  labs(title = "Correlation between errors across models")
