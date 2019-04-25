## Quantifying bellwetheriness: what's the seat distribution in the event that district ___ is won by party ___?
get_win_indices <- function(district_code, party) {
  ## Finds the indices in the simulation data corresponding to a win for [party] in district [district_code]
  
  # Identify what row of the simulation results to use
  district_index <- which(simulation_data$district_code == district_code)
  
  # Now pick the simulation data
  if(grepl("liberal|lpc", party, ignore.case = TRUE)) {
    simulation_results <- LPC_wins
  } else if(grepl("conser|cpc", party, ignore.case = TRUE)) {
    simulation_results <- CPC_wins
  } else if(grepl("ndp", party, ignore.case = TRUE)) {
    simulation_results <- NDP_wins
  } else if(grepl("bloc", party, ignore.case = TRUE)) {
    simulation_results <- Bloc_wins
  } else if(grepl("green|gpc", party, ignore.case = TRUE)) {
    simulation_results <- Green_wins
  }
  
  # Select row of simulation data
  district_sims <- simulation_results[district_index,]
  
  # Extract indices where win == TRUE
  win_indices <- which(district_sims)
  
  return(win_indices)
}

conditional_sims <- function(district_code, party) {
  ## Subsets the simulations down to those where [party] wins [district_code]
  conditional_simulations <- seat_simulations[get_win_indices(district_code, party),]
  
  return(conditional_simulations)
}

conditional_majority_prob <- function(district_code, party, include_type = FALSE) {
  ## Calculates probabilities conditional on [party] winning [district_code]
  require(dplyr)
  
  # Subset simulations data
  simulations <- conditional_sims(district_code, party)
  
  # Create tibble
  if(include_type) {
    prob_tbl <- simulations %>%
      group_by(most_seats, type_of_win) %>%
      summarise(prob = n() / nrow(.)) %>%
      spread(type_of_win, prob)
  } else if(!include_type) {
    prob_tbl <- simulations %>%
      group_by(most_seats) %>%
      summarise(prob = n() / nrow(.))
  }
  
  return(prob_tbl)
}

all_districts_bellwetheriness <- function(party, district_inds) {
  ## Compute bellwetheriness for all districts in [district_inds]
  
  district_codes <- simulation_data$district_code[district_inds]
  
  # Loop through these districts and compute bellwetheriness for each
  bellwetherinesses <- rep(NA, length(district_codes))
  
  for(i in 1:length(district_codes)) {
    bellwetherinesses[i] <- conditional_majority_prob(district_codes[i], party) %>%
      filter(most_seats == party) %>%
      pull(prob)
  }
  
  return(bellwetherinesses)
}
