## make_waterfall_data: function that creates a data frame for waterfall plotting
make_waterfall_data <- function(district_selection, party, models = models_list, data = data_2019) {
  require(tidyverse)
  ## Subset data to the district of interest
  if(is.numeric(district_selection)) {
    district_data.2019 <- data_2019 %>%
      filter(district_code == district_selection) 
    district_name <- district_data.2019$name_english
  } else if(is.character(district_selection)) {
    district_data.2019 <- data_2019 %>%
      filter(name_english == district_selection)
    district_name <- district_data.2019$name_english
  }
  
  ## Extract coefficients from model list
  coefs.LPC <- coef(models$LPC)
  coefs.CPC <- coef(models$CPC)
  coefs.NDP <- coef(models$NDP)
  coefs.Green <- coef(models$Green)
  coefs.Bloc <- coef(models$Bloc)
  
  ## Now do a separate one for each party
  if(party == "Liberal") {
    waterfall_data_ungrouped <- tibble(variable = c(names(coefs.LPC)[1:2], "incumbent_CPC", "incumbent_NDP", "incumbent_Green", "incumbent_Bloc",
                                                    names(coefs.LPC)[4:13])) %>%
      mutate(coefficient = coefs.LPC[variable],
             value = c(1,
                       district_data.2019$incumbent == "Bloc",
                       district_data.2019$incumbent == "Conservative",
                       district_data.2019$incumbent == "Green",
                       district_data.2019$incumbent == "Liberal",
                       district_data.2019$incumbent == "NDP",
                       district_data.2019$LPC_lag,
                       district_data.2019$CPC_lag,
                       district_data.2019$NDP_lag,
                       district_data.2019$Green_lag,
                       district_data.2019$Bloc_lag,
                       district_data.2019$Quebec,
                       district_data.2019$LPC_nation - district_data.2019$LPC_nation_lag,
                       district_data.2019$LPC_region - district_data.2019$LPC_region_lag,
                       district_data.2019$educ_university,
                       district_data.2019$minority),
             variable_group = c("Baseline",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Quebec",
                                "National swing",
                                "Regional swing",
                                "University education",
                                "Visible minority"),
             effect = coefficient*value)
    
    waterfall_data <- waterfall_data_ungrouped %>%
      na.omit() %>%
      group_by(variable_group) %>%
      summarise(effect = 100*sum(effect)) %>%
      arrange(desc(abs(effect + (variable_group == "Baseline")*1000))) %>%
      filter(effect != 0) %>%
      mutate(cumulative_effect = cumsum(effect),
             description = case_when( 
               variable_group == "Baseline" ~ paste0("The Liberal Party baseline is ", round(effect, 1), "% of the vote."),
               variable_group == "Incumbency" & sum(waterfall_data_ungrouped$value[2:6]) == 0 & district_selection != "Beauce" ~ 
                 "The incumbent is not running for reelection.",
               variable_group == "Incumbency" & sum(waterfall_data_ungrouped$value[2:6]) == 0 & district_selection == "Beauce" ~ 
                 "The People's Party incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[2,3]) ~ "The Bloc Québécois incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[3,3]) ~ "The Conservative incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[4,3]) ~ "The Green incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[5,3]) ~ "The Liberal incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[6,3]) ~ "The NDP incumbent is running for reelection.",
               variable_group == "Last election result" ~ paste0("The Liberal candidate won ", round(100*as.numeric(waterfall_data_ungrouped[7,3]), 1),
                                                                 "% of the vote in 2015."),
               variable_group == "Quebec" & as.logical(waterfall_data_ungrouped[12,3]) ~ "This riding is in Quebec.",
               variable_group == "Quebec" & !as.logical(waterfall_data_ungrouped[12,3]) ~ "This riding is not in Quebec.",
               variable_group == "National swing" & (as.numeric(waterfall_data_ungrouped[13,3]) < 0) ~ 
                 paste0("According to polls, the Liberal share of the nationwide vote will decrease by ",
                        -round(100*as.numeric(waterfall_data_ungrouped[13,3]), 1), " percentage points."),
               variable_group == "National swing" & (as.numeric(waterfall_data_ungrouped[13,3]) > 0) ~ 
                 paste0("According to polls, the Liberal share of the nationwide vote will increase by ",
                        round(100*as.numeric(waterfall_data_ungrouped[13,3]), 1), " percentage points."),
               variable_group == "Regional swing" & (as.numeric(waterfall_data_ungrouped[14,3]) < 0) ~
                 paste0("According to polls, the Liberal share of the regional vote will decrease by ",
                        -round(100*as.numeric(waterfall_data_ungrouped[14,3]), 1), " percentage points."),
               variable_group == "Regional swing" & (as.numeric(waterfall_data_ungrouped[14,3]) > 0) ~
                 paste0("According to polls, the Liberal share of the regional vote will increase by ",
                        round(100*as.numeric(waterfall_data_ungrouped[14,3]), 1), " percentage points."),
               variable_group == "University education" ~ paste0(round(100*as.numeric(waterfall_data_ungrouped[15,3]), 1), 
                                                                 "% of the adult population has a university diploma."),
               variable_group == "Visible minority" ~ paste0(round(100*as.numeric(waterfall_data_ungrouped[16,3]), 1), 
                                                             "% of the population are visible minorities.")
             ),
             description = case_when(effect > 0 ~ paste0(description, " (+", round(effect, 1), ")"),
                                     effect < 0 ~ paste0(description, " (", round(effect, 1), ")"),
                                     effect == 0 ~ paste0(description, " (+0)")),
             previous_cumulative_effect = lag(cumulative_effect),
             previous_cumulative_effect = case_when(is.na(previous_cumulative_effect) ~ 0,
                                                    !is.na(previous_cumulative_effect) ~ previous_cumulative_effect)
      ) %>%
      arrange(desc(abs(effect) + (variable_group == "Baseline")*1000)) %>%
      mutate(order = n():1)
    
    waterfall_data$variable_group <- ordered(waterfall_data$variable_group, levels = rev(waterfall_data$variable_group))
  }
  
  if(party == "Conservative") {
    waterfall_data_ungrouped <- tibble(variable = c(names(coefs.CPC)[1:3], "incumbent_NDPTRUE", "incumbent_GreenTRUE", "incumbent_BlocTRUE",
                                                    names(coefs.CPC)[4:11])) %>%
      mutate(coefficient = coefs.CPC[variable],
             value = c(1,
                       district_data.2019$incumbent == "Liberal",
                       district_data.2019$incumbent == "Conservative",
                       district_data.2019$incumbent == "NDP",
                       district_data.2019$incumbent == "Green",
                       district_data.2019$incumbent == "Bloc",
                       district_data.2019$LPC_lag,
                       district_data.2019$CPC_lag,
                       district_data.2019$NDP_lag,
                       district_data.2019$Green_lag,
                       district_data.2019$Bloc_lag,
                       district_data.2019$CPC_nation,
                       district_data.2019$CPC_region - district_data.2019$CPC_region_lag,
                       district_data.2019$minority),
             variable_group = c("Baseline",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Nationwide vote",
                                "Regional swing",
                                "Visible minority"),
             effect = coefficient*value)
    
    waterfall_data <- waterfall_data_ungrouped %>%
      na.omit() %>%
      group_by(variable_group) %>%
      summarise(effect = 100*sum(effect)) %>%
      arrange(desc(abs(effect + (variable_group == "Baseline")*1000))) %>%
      filter(effect != 0) %>%
      mutate(cumulative_effect = cumsum(effect),
             description = case_when(
               variable_group == "Baseline" ~ paste0("The Conservative Party baseline is ", round(effect, 1), "% of the vote."),
               variable_group == "Incumbency" & sum(waterfall_data_ungrouped$value[2:3]) == 0 & district_name != "Beauce" ~ 
                 "The incumbent is not running for reelection.",
               variable_group == "Incumbency" & sum(waterfall_data_ungrouped$value[2:3]) == 0 & district_name == "Beauce" ~ 
                 "The Peoples Party incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[2,3]) ~ "The Liberal incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[3,3]) ~ "The Conservative incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[4,3]) ~ "The NDP incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[5,3]) ~ "The Green Party incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[6,3]) ~ "The Bloc Québécois incumbent is running for reelection.",
               variable_group == "Last election result" ~ paste0("The Conservative candidate won ", round(100*as.numeric(waterfall_data_ungrouped[8,3]), 1),
                                                                 "% of the vote in 2015."),
               variable_group == "Nationwide vote" ~ paste0("According to polls, the Conservatives will win ", 
                                                            round(100*as.numeric(waterfall_data_ungrouped[12,3]), 1), "% of the nationwide vote."),
               variable_group == "Regional swing" & (as.numeric(waterfall_data_ungrouped[13,3]) < 0) ~
                 paste0("According to polls, the Conservative share of the regional vote will decrease by ",
                        -round(100*as.numeric(waterfall_data_ungrouped[13,3]), 1), " percentage points."),
               variable_group == "Regional swing" & (as.numeric(waterfall_data_ungrouped[13,3]) > 0) ~
                 paste0("According to polls, the Conservative share of the regional vote will increase by ",
                        round(100*as.numeric(waterfall_data_ungrouped[13,3]), 1), " percentage points."),
               variable_group == "Visible minority" ~ paste0(round(100*as.numeric(waterfall_data_ungrouped[14,3]), 1), 
                                                             "% of the population are visible minorities.")
             ),
             description = case_when(effect > 0 ~ paste0(description, " (+", round(effect, 1), ")"),
                                     effect < 0 ~ paste0(description, " (", round(effect, 1), ")"),
                                     effect == 0 ~ paste0(description, " (+0)")),
             previous_cumulative_effect = lag(cumulative_effect),
             previous_cumulative_effect = case_when(is.na(previous_cumulative_effect) ~ 0,
                                                    !is.na(previous_cumulative_effect) ~ previous_cumulative_effect)
      ) %>%
      arrange(desc(abs(effect) + (variable_group == "Baseline")*1000)) %>%
      mutate(order = n():1)
    
    waterfall_data$variable_group <- ordered(waterfall_data$variable_group, levels = rev(waterfall_data$variable_group))
  }
  
  if(party == "NDP") {
    waterfall_data_ungrouped <- tibble(variable = c(names(coefs.NDP)[1:4], "incumbent_GreenTRUE", "incumbent_BlocTRUE",
                                                    names(coefs.NDP)[5:12])) %>%
      mutate(coefficient = coefs.NDP[variable],
             value = c(1,
                       district_data.2019$incumbent == "Liberal",
                       district_data.2019$incumbent == "Conservative",
                       district_data.2019$incumbent == "NDP",
                       district_data.2019$incumbent == "Green",
                       district_data.2019$incumbent == "Bloc",
                       district_data.2019$LPC_lag,
                       district_data.2019$CPC_lag,
                       district_data.2019$NDP_lag,
                       district_data.2019$Green_lag,
                       district_data.2019$Bloc_lag,
                       district_data.2019$NDP_nation - district_data.2019$NDP_nation_lag,
                       district_data.2019$NDP_region - district_data.2019$NDP_region_lag,
                       district_data.2019$age_65),
             variable_group = c("Baseline",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "National swing",
                                "Regional swing",
                                "Retiree population"),
             effect = coefficient*value)
    
    waterfall_data <- waterfall_data_ungrouped %>%
      na.omit() %>%
      group_by(variable_group) %>%
      summarise(effect = 100*sum(effect)) %>%
      arrange(desc(abs(effect + (variable_group == "Baseline")*1000))) %>%
      filter(effect != 0) %>%
      mutate(cumulative_effect = cumsum(effect),
             description = case_when( 
               variable_group == "Baseline" ~ paste0("The NDP baseline is ", round(effect, 1), "% of the vote."),
               variable_group == "Incumbency" & sum(waterfall_data_ungrouped$value[2:6]) == 0 & district_selection != "Beauce" ~ 
                 "The incumbent is not running for reelection.",
               variable_group == "Incumbency" & sum(waterfall_data_ungrouped$value[2:6]) == 0 & district_selection == "Beauce" ~ 
                 "The Peoples Party incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[2,3]) ~ "The Liberal incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[3,3]) ~ "The Conservative incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[4,3]) ~ "The NDP incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[5,3]) ~ "The Green Party incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[6,3]) ~ "The Bloc Québécois incumbent is running for reelection.",
               variable_group == "Last election result" ~ paste0("The NDP candidate won ", round(100*as.numeric(waterfall_data_ungrouped[9,3]), 1),
                                                                 "% of the vote in 2015."),
               variable_group == "National swing" & (as.numeric(waterfall_data_ungrouped[12,3]) < 0) ~ 
                 paste0("According to polls, the NDP share of the nationwide vote will decrease by ",
                        -round(100*as.numeric(waterfall_data_ungrouped[12,3]), 1), " percentage points."),
               variable_group == "National swing" & (as.numeric(waterfall_data_ungrouped[12,3]) > 0) ~ 
                 paste0("According to polls, the NDP share of the nationwide vote will increase by ",
                        round(100*as.numeric(waterfall_data_ungrouped[12,3]), 1), " percentage points."),
               variable_group == "Regional swing" & (as.numeric(waterfall_data_ungrouped[13,3]) < 0) ~
                 paste0("According to polls, the NDP share of the regional vote will decrease by ",
                        -round(100*as.numeric(waterfall_data_ungrouped[13,3]), 1), " percentage points."),
               variable_group == "Regional swing" & (as.numeric(waterfall_data_ungrouped[13,3]) > 0) ~
                 paste0("According to polls, the NDP share of the regional vote will increase by ",
                        round(100*as.numeric(waterfall_data_ungrouped[13,3]), 1), " percentage points."),
               variable_group == "Retiree population" ~ paste0(round(100*as.numeric(waterfall_data_ungrouped[14,3]), 1), 
                                                               "% of the population is over the age of 65.")
             ),
             description = case_when(effect > 0 ~ paste0(description, " (+", round(effect, 1), ")"),
                                     effect < 0 ~ paste0(description, " (", round(effect, 1), ")"),
                                     effect == 0 ~ paste0(description, " (+0)")),
             previous_cumulative_effect = lag(cumulative_effect),
             previous_cumulative_effect = case_when(is.na(previous_cumulative_effect) ~ 0,
                                                    !is.na(previous_cumulative_effect) ~ previous_cumulative_effect)
      ) %>%
      arrange(desc(abs(effect) + (variable_group == "Baseline")*1000)) %>%
      mutate(order = n():1)
    
    waterfall_data$variable_group <- ordered(waterfall_data$variable_group, levels = rev(waterfall_data$variable_group))
  }
  
  if(party == "Green") {
    waterfall_data_ungrouped <- tibble(variable = names(coefs.Green)) %>%
      mutate(coefficient = coefs.Green[variable],
             value = c(1,
                       district_data.2019$incumbent == "Liberal",
                       district_data.2019$incumbent == "Conservative",
                       district_data.2019$incumbent == "NDP",
                       district_data.2019$incumbent == "Green",
                       district_data.2019$incumbent == "Bloc",
                       district_data.2019$LPC_lag,
                       district_data.2019$CPC_lag,
                       district_data.2019$NDP_lag,
                       district_data.2019$Green_lag,
                       district_data.2019$Bloc_lag,
                       district_data.2019$Green_nation - district_data.2019$Green_nation_lag,
                       district_data.2019$Green_region - district_data.2019$Green_region_lag,
                       district_data.2019$minority,
                       district_data.2019$educ_university),
             variable_group = c("Baseline",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "National swing",
                                "Regional swing",
                                "Visible minority",
                                "University education"),
             effect = coefficient*value)
    
    waterfall_data <- waterfall_data_ungrouped %>%
      na.omit() %>%
      group_by(variable_group) %>%
      summarise(effect = 100*sum(effect)) %>%
      arrange(desc(abs(effect + (variable_group == "Baseline")*1000))) %>%
      filter(effect != 0) %>%
      mutate(cumulative_effect = cumsum(effect),
             description = case_when( 
               variable_group == "Baseline" ~ paste0("The Green Party baseline is ", round(effect, 1), "% of the vote."),
               variable_group == "Incumbency" & sum(waterfall_data_ungrouped$value[2:6]) == 0 & district_selection != "Beauce" ~ 
                 "The incumbent is not running for reelection.",
               variable_group == "Incumbency" & sum(waterfall_data_ungrouped$value[2:6]) == 0 & district_selection == "Beauce" ~ 
                 "The Peoples Party incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[2,3]) ~ "The Liberal incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[3,3]) ~ "The Conservative incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[4,3]) ~ "The NDP incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[5,3]) ~ "The Green Party incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[6,3]) ~ "The Bloc Québécois incumbent is running for reelection.",
               variable_group == "Last election result" ~ paste0("The Green Party candidate won ", round(100*as.numeric(waterfall_data_ungrouped[10,3]), 1),
                                                                 "% of the vote in 2015."),
               variable_group == "National swing" & (as.numeric(waterfall_data_ungrouped[12,3]) < 0) ~ 
                 paste0("According to polls, the Green Party share of the nationwide vote will decrease by ",
                        -round(100*as.numeric(waterfall_data_ungrouped[12,3]), 1), " percentage points."),
               variable_group == "National swing" & (as.numeric(waterfall_data_ungrouped[12,3]) > 0) ~ 
                 paste0("According to polls, the Green Party share of the nationwide vote will increase by ",
                        round(100*as.numeric(waterfall_data_ungrouped[12,3]), 1), " percentage points."),
               variable_group == "Regional swing" & (as.numeric(waterfall_data_ungrouped[13,3]) < 0) ~
                 paste0("According to polls, the Green Party share of the regional vote will decrease by ",
                        -round(100*as.numeric(waterfall_data_ungrouped[13,3]), 1), " percentage points."),
               variable_group == "Regional swing" & (as.numeric(waterfall_data_ungrouped[13,3]) > 0) ~
                 paste0("According to polls, the Green Party share of the regional vote will increase by ",
                        round(100*as.numeric(waterfall_data_ungrouped[13,3]), 1), " percentage points."),
               variable_group == "Visible minority" ~ paste0(round(100*as.numeric(waterfall_data_ungrouped[14,3]), 1), 
                                                             "% of the population are visible minorities."),
               variable_group == "University education" ~ paste0(round(100*as.numeric(waterfall_data_ungrouped[15,3]), 1),
                                                                 "% of the adult population has a university diploma.")
             ),
             description = case_when(effect > 0 ~ paste0(description, " (+", round(effect, 1), ")"),
                                     effect < 0 ~ paste0(description, " (", round(effect, 1), ")"),
                                     effect == 0 ~ paste0(description, " (+0)")),
             previous_cumulative_effect = lag(cumulative_effect),
             previous_cumulative_effect = case_when(is.na(previous_cumulative_effect) ~ 0,
                                                    !is.na(previous_cumulative_effect) ~ previous_cumulative_effect)
      ) %>%
      arrange(desc(abs(effect) + (variable_group == "Baseline")*1000)) %>%
      mutate(order = n():1)
    
    waterfall_data$variable_group <- ordered(waterfall_data$variable_group, levels = rev(waterfall_data$variable_group))
  }
  
  if(party == "Bloc" & district_data.2019$province == "Quebec") {
    waterfall_data_ungrouped <- tibble(variable = c(names(coefs.Bloc)[1:5], "incumbent_GreenTRUE", names(coefs.Bloc)[6:12])) %>%
      mutate(coefficient = coefs.Bloc[variable],
             value = c(1,
                       district_data.2019$incumbent == "Liberal",
                       district_data.2019$incumbent == "Conservative",
                       district_data.2019$incumbent == "NDP",
                       district_data.2019$incumbent == "Bloc",
                       district_data.2019$incumbent == "Green",
                       district_data.2019$LPC_lag,
                       district_data.2019$CPC_lag,
                       district_data.2019$NDP_lag,
                       district_data.2019$Green_lag,
                       district_data.2019$Bloc_lag,
                       district_data.2019$Bloc_nation - district_data.2019$Bloc_nation_lag,
                       district_data.2019$Bloc_region - district_data.2019$Bloc_region_lag),
             variable_group = c("Baseline",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Incumbency",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "Last election result",
                                "National swing",
                                "Regional swing"),
             effect = coefficient*value)
    
    waterfall_data <- waterfall_data_ungrouped %>%
      na.omit() %>%
      group_by(variable_group) %>%
      summarise(effect = 100*sum(effect)) %>%
      arrange(desc(abs(effect + (variable_group == "Baseline")*1000))) %>%
      filter(effect != 0) %>%
      mutate(cumulative_effect = cumsum(effect),
             description = case_when( 
               variable_group == "Baseline" ~ paste0("The Bloc Québécois baseline is ", round(effect, 1), "% of the vote."),
               variable_group == "Incumbency" & sum(waterfall_data_ungrouped$value[2:6]) == 0 & district_selection != "Beauce" ~ 
                 "The incumbent is not running for reelection.",
               variable_group == "Incumbency" & sum(waterfall_data_ungrouped$value[2:6]) == 0 & district_selection == "Beauce" ~ 
                 "The Peoples Party incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[2,3]) ~ "The Liberal incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[3,3]) ~ "The Conservative incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[4,3]) ~ "The NDP incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[5,3]) ~ "The Bloc Québécois incumbent is running for reelection.",
               variable_group == "Incumbency" & as.logical(waterfall_data_ungrouped[6,3]) ~ "The Green Party incumbent is running for reelection.",
               variable_group == "Last election result" ~ paste0("The Bloc Québécois candidate won ", round(100*as.numeric(waterfall_data_ungrouped[11,3]), 1),
                                                                 "% of the vote in 2015."),
               variable_group == "National swing" & (as.numeric(waterfall_data_ungrouped[12,3]) < 0) ~ 
                 paste0("According to polls, the Bloc Québécois share of the nationwide vote will decrease by ",
                        -round(100*as.numeric(waterfall_data_ungrouped[12,3]), 1), " percentage points."),
               variable_group == "National swing" & (as.numeric(waterfall_data_ungrouped[12,3]) > 0) ~ 
                 paste0("According to polls, the Bloc Québécois share of the nationwide vote will increase by ",
                        round(100*as.numeric(waterfall_data_ungrouped[12,3]), 1), " percentage points."),
               variable_group == "Regional swing" & (as.numeric(waterfall_data_ungrouped[13,3]) < 0) ~
                 paste0("According to polls, the Bloc Québécois share of the regional vote will decrease by ",
                        -round(100*as.numeric(waterfall_data_ungrouped[13,3]), 1), " percentage points."),
               variable_group == "Regional swing" & (as.numeric(waterfall_data_ungrouped[13,3]) > 0) ~
                 paste0("According to polls, the Bloc Québécois share of the regional vote will increase by ",
                        round(100*as.numeric(waterfall_data_ungrouped[13,3]), 1), " percentage points.")
             ),
             description = case_when(effect > 0 ~ paste0(description, " (+", round(effect, 1), ")"),
                                     effect < 0 ~ paste0(description, " (", round(effect, 1), ")"),
                                     effect == 0 ~ paste0(description, " (+0)")),
             previous_cumulative_effect = lag(cumulative_effect),
             previous_cumulative_effect = case_when(is.na(previous_cumulative_effect) ~ 0,
                                                    !is.na(previous_cumulative_effect) ~ previous_cumulative_effect)
      ) %>%
      arrange(desc(abs(effect) + (variable_group == "Baseline")*1000)) %>%
      mutate(order = n():1)
    
    waterfall_data$variable_group <- ordered(waterfall_data$variable_group, levels = rev(waterfall_data$variable_group))
    
  } else if(party == "Bloc" & district_data.2019$province != "Quebec") {
    waterfall_data <- tibble(variable_group = "Quebec", 
                             effect = 0, 
                             cumulative_effect = 0,
                             description = "This riding is not in Quebec.",
                             previous_cumulative_effect = 0, 
                             order = 1)
    
    waterfall_data$variable_group <- ordered(waterfall_data$variable_group, levels = rev(waterfall_data$variable_group))
  }
  
  waterfall_data <- waterfall_data %>%
    mutate(district = district_name,
           party = party)
  
  return(waterfall_data)
}

## make_waterfall_plot: takes data frame created by make_waterfall_data and 
make_waterfall_plot <- function(waterfall_data) {
  require(tidyverse)
  require(ggiraph)
  waterfall_plot <- suppressWarnings(ggplot(waterfall_data, aes(variable_group, fill = factor(sign(effect)))) +
    geom_hline(data = waterfall_data %>% 
                 tail(1) %>% 
                 mutate(description = paste0("The ", party, " candidate is projected to win ", cumulative_effect, "% of the vote.")), 
               aes(yintercept = cumulative_effect, col = variable_group), show.legend = FALSE) +
    geom_rect_interactive(aes(x = variable_group, xmin = order - 0.5, xmax = order + 0.5, ymin = previous_cumulative_effect, ymax = cumulative_effect,
                              tooltip = description), col = "black") +
    coord_flip() +
    scale_fill_manual(name = "Effect direction", labels = c("Negative", "Positive"), values = c("red", "green4")) +
    scale_colour_manual(name = "", labels = paste0("Predicted ", waterfall_data$party[1],  " %"), values = "black") +
    labs(title = paste0("Forecast components for the ", waterfall_data$party[1], " share of the vote"),
         subtitle = waterfall_data$district[1], x = "Predictor", y = "Cumulative effect (percentage points)")
  )
  return(waterfall_plot)
}

