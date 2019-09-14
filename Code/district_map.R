source("Code/library.R")

## Create Lambert conformal conic CRS for leaflet (see http://spatialreference.org/ref/esri/canada-lambert-conformal-conic/ )
crs_proj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
crs_lcc <- leafletCRS(code = "ESRI:102002", proj4def = crs_proj)

## Read shapefile
canada_districts <- readOGR(dsn = "Data/Shapefiles", layer = "FED_CA_2_2_ENG") %>%
  ms_simplify()

## Make predictions
preds_data <- data_2019.simple %>%
  mutate(LPC_pred = predict(model_LPC.linear, newdata = .),
         CPC_pred = predict(model_CPC.linear, newdata = .),
         NDP_pred = predict(model_NDP.linear, newdata = .),
         Green_pred = predict(model_Green.linear, newdata = .) + Green_lag,
         Bloc_pred = (province == "Quebec")*predict(model_Bloc.linear, newdata = .)) %>%
  mutate_all(function(x) pmax(x, 0)) %>%
  mutate(LPC_rho = LPC_rho,
         CPC_rho = CPC_rho,
         NDP_rho = NDP_rho,
         Green_rho = Green_rho,
         Bloc_rho = Bloc_rho,
         ind_rho = ind_rho,
         CPC_pred = case_when(district_code == 24007 ~ (17.09/(67.02 - 6.64))*CPC_pred,
                              district_code != 24007 ~ CPC_pred),
         LPC_pred = case_when(district_code == 35054 ~ (1 - invlogit(philpott_mean.logit))*LPC_pred,
                              district_code == 59036 ~ (1 - invlogit(wilson_raybould_mean.logit))*LPC_pred,
                              (district_code != 35054) & (district_code != 59036) ~ LPC_pred),
         ind_pred = case_when(district_code == 35054 ~ invlogit(philpott_mean.logit)*LPC_pred + 0.1*NDP_pred,
                              district_code == 59036 ~ invlogit(wilson_raybould_mean.logit)*LPC_pred + 0.1*NDP_pred),
         NDP_pred = case_when(district_code %in% c(35054, 59036) ~ 0.9*NDP_pred,
                              !(district_code %in% c(35054, 59036)) ~ NDP_pred)
  ) %>%
  left_join(district_poll_avg, by = "district_code") %>%
  mutate_all(function(x) {
    x[is.na(x)] <- 0
    return(x)
    }) %>%
  mutate(LPC_pred = LPC_rho*LPC_pred + (1 - LPC_rho)*LPC_poll,
         CPC_pred = CPC_rho*CPC_pred + (1 - CPC_rho)*CPC_poll,
         NDP_pred = NDP_rho*NDP_pred + (1 - NDP_rho)*NDP_poll,
         Bloc_pred = Bloc_rho*Bloc_pred + (1 - Bloc_rho)*BQ_poll,
         Green_pred = Green_rho*Green_pred + (1 - Green_rho)*GPC_poll,
         ind_pred = ind_rho*ind_pred + (1 - ind_rho)*Ind_poll) %>%
  dplyr::select(district_code, LPC_pred, CPC_pred, NDP_pred, Green_pred, Bloc_pred, ind_pred)

## Transform to lat-long
canada_districts_latlong <- spTransform(canada_districts, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  st_as_sf() %>%
  merge(district_key_2013, by.x = "FED_NUM", by.y = "district_code", all = FALSE) %>%
  merge(province_key, by.x = "PROVCODE", by.y = "province_abbr", all.x = TRUE) %>%
  merge(preds_data, by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(LPC_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(CPC_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(NDP_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(Bloc_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(Green_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(PPC_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(ind_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(district_probs %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(read_csv("Data/incumbents.csv") %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(read_csv("Data/candidates_2019.csv") %>% 
          mutate(candidate = case_when(is.na(candidate_last) ~ "TBD",
                                       !is.na(candidate_last) ~ paste(candidate_first, candidate_last))) %>%
          dplyr::select(district_code, party, candidate) %>%
          distinct(district_code, party, .keep_all = TRUE) %>%
          spread(party, candidate) %>%
          replace_na(list(district_code = NA, Bloc = "TBD", Conservative = "TBD", Green = "TBD", Liberal = "TBD", NDP = "TBD", `People's` = "TBD", 
                          Independent = "")) %>%
          dplyr::select(district_code, LPC_cand = Liberal, CPC_cand = Conservative, NDP_cand = NDP, Green_cand = Green, Bloc_cand = Bloc, 
                        PPC_cand = `People's`, ind_cand = Independent), 
        by.x = "FED_NUM", by.y = "district_code") %>%
  as.data.frame() %>%
  mutate(max_prob = pmax(LPC_prob, CPC_prob, NDP_prob, Bloc_prob, Green_prob, PPC_prob, ind_prob),
         predicted_winner = case_when(LPC_prob == max_prob ~ "Liberal",
                                      CPC_prob == max_prob ~ "Conservative",
                                      NDP_prob == max_prob ~ "NDP",
                                      Bloc_prob == max_prob ~ "Bloc",
                                      Green_prob == max_prob ~ "Green",
                                      PPC_prob == max_prob ~ "People's Party",
                                      ind_prob == max_prob ~ "Independent"),
         last_color = case_when(last_winner == "Liberal" ~ "'red'",
                                last_winner == "Conservative" ~ "'blue'",
                                last_winner == "NDP" ~ "'#EE7600'",
                                last_winner == "Bloc" ~ "'#8B008B'",
                                last_winner == "Green" ~ "'#008B00'",
                                last_winner == "People's Party" ~ "'#191970'",
                                FED_NUM %in% c(35054, 59036) ~ "'#444444'"),
         predicted_color = case_when(predicted_winner == "Liberal" ~ "'red'",
                                     predicted_winner == "Conservative" ~ "'blue'",
                                     predicted_winner == "NDP" ~ "'#EE7600'",
                                     predicted_winner == "Bloc" ~ "'#8B008B'",
                                     predicted_winner == "Green" ~ "'#008B00'",
                                     predicted_winner == "People's Party" ~ "'#191970'",
                                     predicted_winner == "Independent" ~ "'#444444'"),
         hold = (predicted_winner == last_winner),
         fill_color = case_when(LPC_prob == max_prob ~ "red",
                                CPC_prob == max_prob ~ "blue",
                                NDP_prob == max_prob ~ "#EE7600",
                                Bloc_prob == max_prob ~ "#8B008B",
                                Green_prob == max_prob ~ "#008B00",
                                PPC_prob == max_prob ~ "#191970",
                                ind_prob == max_prob ~ "'#444444'"),
         FED_NUM = as.character(FED_NUM),
         district_info = case_when(
           ## Holds in Quebec
           PROVCODE == "QC" & name_english != "Beauce" & predicted_winner == last_winner ~
             paste0("<b><u><font size = 2.5>", name_english, "</font></b></u><br>",
                    "<b><font color = ", predicted_color, ">", predicted_winner, "</font> hold</b><br>",
                    "<br>",
                    "<b><u>Win probabilities</u></b><br>",
                    "<font color = 'red'><b>", LPC_cand, "</b> (LPC)</font>: ", round(100*LPC_prob), "%<br>",
                    "<font color = 'blue'><b>", CPC_cand, "</b> (CPC)</font>: ", round(100*CPC_prob), "%<br>",
                    "<font color = '#EE7600'><b>", NDP_cand, "</b> (NDP)</font>: ", round(100*NDP_prob), "%<br>",
                    "<font color = '#8B008B'><b>", Bloc_cand, "</b> (Bloc)</font>: ", round(100*Bloc_prob), "%<br>",
                    "<font color = '#008B00'><b>", Green_cand, "</b> (Green)</font>: ", round(100*Green_prob), "%<br>",
                    "<br>",
                    "<b><u>Projected vote (90% CI)</u></b><br>",
                    "<font color = 'red'><b>LPC</b></font>: ", round(100*LPC_pred, 1), "% (", round(100*pct_05.LPC, 1), "%–", round(100*pct_95.LPC, 1), "%)<br>",
                    "<font color = 'blue'><b>CPC</b></font>: ", round(100*CPC_pred, 1), "% (", round(100*pct_05.CPC, 1), "%–", round(100*pct_95.CPC, 1), "%)<br>",
                    "<font color = '#EE7600'><b>NDP</b></font>: ", round(100*NDP_pred, 1), "% (", round(100*pct_05.NDP, 1), "%–", round(100*pct_95.NDP, 1), "%)<br>",
                    "<font color = '#8B008B'><b>Bloc</b></font>: ", round(100*Bloc_pred, 1), "% (", round(100*pct_05.Bloc, 1), "%–", round(100*pct_95.Bloc, 1), "%)<br>",
                    "<font color = '#008B00'><b>Green</b></font>: ", round(100*Green_pred, 1), "% (", round(100*pct_05.Green, 1), "%–", round(100*pct_95.Green, 1), "%)"),
           
           ## Flips in Quebec
           PROVCODE == "QC" & name_english != "Beauce" & predicted_winner != last_winner ~
             paste0("<b><u><font size = 2.5>", name_english, "</font></b></u><br>",
                    "<b><font color = ", predicted_color, ">", predicted_winner, "</font> gain from ", "<font color = ", last_color, ">", last_winner, "</font></b><br>",
                    "<br>",
                    "<b><u>Win probabilities</u></b><br>",
                    "<font color = 'red'><b>", LPC_cand, "</b> (LPC)</font>: ", round(100*LPC_prob), "%<br>",
                    "<font color = 'blue'><b>", CPC_cand, "</b> (CPC)</font>: ", round(100*CPC_prob), "%<br>",
                    "<font color = '#EE7600'><b>", NDP_cand, "</b> (NDP)</font>: ", round(100*NDP_prob), "%<br>",
                    "<font color = '#8B008B'><b>", Bloc_cand, "</b> (Bloc)</font>: ", round(100*Bloc_prob), "%<br>",
                    "<font color = '#008B00'><b>", Green_cand, "</b> (Green)</font>: ", round(100*Green_prob), "%<br>",
                    "<br>",
                    "<b><u>Projected vote (90% CI)</u></b><br>",
                    "<font color = 'red'><b>LPC</b></font>: ", round(100*LPC_pred, 1), "% (", round(100*pct_05.LPC, 1), "%–", round(100*pct_95.LPC, 1), "%)<br>",
                    "<font color = 'blue'><b>CPC</b></font>: ", round(100*CPC_pred, 1), "% (", round(100*pct_05.CPC, 1), "%–", round(100*pct_95.CPC, 1), "%)<br>",
                    "<font color = '#EE7600'><b>NDP</b></font>: ", round(100*NDP_pred, 1), "% (", round(100*pct_05.NDP, 1), "%–", round(100*pct_95.NDP, 1), "%)<br>",
                    "<font color = '#8B008B'><b>Bloc</b></font>: ", round(100*Bloc_pred, 1), "% (", round(100*pct_05.Bloc, 1), "%–", round(100*pct_95.Bloc, 1), "%)<br>",
                    "<font color = '#008B00'><b>Green</b></font>: ", round(100*Green_pred, 1), "% (", round(100*pct_05.Green, 1), "%–", round(100*pct_95.Green, 1), "%)"
                    ),
           
           ## Beauce (Maxime Bernier)
           name_english == "Beauce" & predicted_winner == last_winner ~
             paste0("<b><u><font size = 2.5>", name_english, "</font></b></u><br>",
                    "<b><font color = ", predicted_color, ">", predicted_winner, "</font> hold</b><br>",
                    "<br>",
                    "<b><u>Win probabilities</u></b><br>",
                    "<font color = 'red'><b>", LPC_cand, "</b> (LPC)</font>: ", round(100*LPC_prob), "%<br>",
                    "<font color = 'blue'><b>", CPC_cand, "</b> (CPC)</font>: ", round(100*CPC_prob), "%<br>",
                    "<font color = '#EE7600'><b>", NDP_cand, "</b> (NDP)</font>: ", round(100*NDP_prob), "%<br>",
                    "<font color = '#8B008B'><b>", Bloc_cand, "</b> (Bloc)</font>: ", round(100*Bloc_prob), "%<br>",
                    "<font color = '#008B00'><b>", Green_cand, "</b> (Green)</font>: ", round(100*Green_prob), "%<br>",
                    "<font color = '#191970'><b>", PPC_cand, "</b> (PPC)</font>: ", round(100*PPC_prob), "%<br>",
                    "<br>",
                    "<b><u>Projected vote (90% CI)</u></b><br>",
                    "<font color = 'red'><b>LPC</b></font>: ", round(100*LPC_pred, 1), "% (", round(100*pct_05.LPC, 1), "%–", round(100*pct_95.LPC, 1), "%)<br>",
                    "<font color = 'blue'><b>CPC</b></font>: ", round(100*CPC_pred, 1), "% (", round(100*pct_05.CPC, 1), "%–", round(100*pct_95.CPC, 1), "%)<br>",
                    "<font color = '#EE7600'><b>NDP</b></font>: ", round(100*NDP_pred, 1), "% (", round(100*pct_05.NDP, 1), "%–", round(100*pct_95.NDP, 1), "%)<br>",
                    "<font color = '#8B008B'><b>Bloc</b></font>: ", round(100*Bloc_pred, 1), "% (", round(100*pct_05.Bloc, 1), "%–", round(100*pct_95.Bloc, 1), "%)<br>",
                    "<font color = '#008B00'><b>Green</b></font>: ", round(100*Green_pred, 1), "% (", round(100*pct_05.Green, 1), "%–", round(100*pct_95.Green, 1), "%)<br>",
                    "<font color = '#191970'><b>PPC</b></font>: ", round(100*pct_50.PPC, 1), "% (", round(100*pct_05.PPC, 1), "%–", round(100*pct_95.PPC, 1), "%)"
                    ),
           
           name_english == "Beauce" & predicted_winner != last_winner ~ 
             paste0("<b><u><font size = 2.5>", name_english, "</font></b></u><br>",
                    "<b><font color = ", predicted_color, ">", predicted_winner, "</font> gain from ", "<font color = ", last_color, ">", last_winner, "</font></b><br>",
                    "<br>",
                    "<b><u>Win probabilities</u></b><br>",
                    "<font color = 'red'><b>", LPC_cand, "</b> (LPC)</font>: ", round(100*LPC_prob), "%<br>",
                    "<font color = 'blue'><b>", CPC_cand, "</b> (CPC)</font>: ", round(100*CPC_prob), "%<br>",
                    "<font color = '#EE7600'><b>", NDP_cand, "</b> (NDP)</font>: ", round(100*NDP_prob), "%<br>",
                    "<font color = '#8B008B'><b>", Bloc_cand, "</b> (Bloc)</font>: ", round(100*Bloc_prob), "%<br>",
                    "<font color = '#008B00'><b>", Green_cand, "</b> (Green)</font>: ", round(100*Green_prob), "%<br>",
                    "<font color = '#191970'><b>", PPC_cand, "</b> (PPC)</font>: ", round(100*PPC_prob), "%<br>",
                    "<br>",
                    "<b><u>Projected vote (90% CI)</u></b><br>",
                    "<font color = 'red'><b>LPC</b></font>: ", round(100*LPC_pred, 1), "% (", round(100*pct_05.LPC, 1), "%–", round(100*pct_95.LPC, 1), "%)<br>",
                    "<font color = 'blue'><b>CPC</b></font>: ", round(100*CPC_pred, 1), "% (", round(100*pct_05.CPC, 1), "%–", round(100*pct_95.CPC, 1), "%)<br>",
                    "<font color = '#EE7600'><b>NDP</b></font>: ", round(100*NDP_pred, 1), "% (", round(100*pct_05.NDP, 1), "%–", round(100*pct_95.NDP, 1), "%)<br>",
                    "<font color = '#8B008B'><b>Bloc</b></font>: ", round(100*Bloc_pred, 1), "% (", round(100*pct_05.Bloc, 1), "%–", round(100*pct_95.Bloc, 1), "%)<br>",
                    "<font color = '#008B00'><b>Green</b></font>: ", round(100*Green_pred, 1), "% (", round(100*pct_05.Green, 1), "%–", round(100*pct_95.Green, 1), "%)<br>",
                    "<font color = '#191970'><b>PPC</b></font>: ", round(100*pct_50.PPC, 1), "% (", round(100*pct_05.PPC, 1), "%–", round(100*pct_95.PPC, 1), "%)"
             ),
           
           ## Holds outside Quebec
           PROVCODE != "QC" & !(last_winner %in% c("Independent", "CCF")) & predicted_winner == last_winner ~
             paste0("<b><u><font size = 2.5>", name_english, "</font></b></u><br>",
                    "<b><font color = ", predicted_color, ">", predicted_winner, "</font> hold</b><br>",
                    "<br>",
                    "<b><u>Win probabilities</u></b><br>",
                    "<font color = 'red'><b>", LPC_cand, "</b> (LPC)</font>: ", round(100*LPC_prob), "%<br>",
                    "<font color = 'blue'><b>", CPC_cand, "</b> (CPC)</font>: ", round(100*CPC_prob), "%<br>",
                    "<font color = '#EE7600'><b>", NDP_cand, "</b> (NDP)</font>: ", round(100*NDP_prob), "%<br>",
                    "<font color = '#008B00'><b>", Green_cand, "</b> (Green)</font>: ", round(100*Green_prob), "%<br>",
                    "<br>",
                    "<b><u>Projected vote (90% CI)</u></b><br>",
                    "<font color = 'red'><b>LPC</b></font>: ", round(100*LPC_pred, 1), "% (", round(100*pct_05.LPC, 1), "%–", round(100*pct_95.LPC, 1), "%)<br>",
                    "<font color = 'blue'><b>CPC</b></font>: ", round(100*CPC_pred, 1), "% (", round(100*pct_05.CPC, 1), "%–", round(100*pct_95.CPC, 1), "%)<br>",
                    "<font color = '#EE7600'><b>NDP</b></font>: ", round(100*NDP_pred, 1), "% (", round(100*pct_05.NDP, 1), "%–", round(100*pct_95.NDP, 1), "%)<br>",
                    "<font color = '#008B00'><b>Green</b></font>: ", round(100*Green_pred, 1), "% (", round(100*pct_05.Green, 1), "%–", round(100*pct_95.Green, 1), "%)"),
           
           ## Flips outside Quebec
           PROVCODE != "QC" & !(last_winner %in% c("Independent", "CCF")) & predicted_winner != last_winner ~
             paste0("<b><u><font size = 2.5>", name_english, "</font></b></u><br>",
                    "<b><font color = ", predicted_color, ">", predicted_winner, "</font> gain from ", "<font color = ", last_color, ">", last_winner, "</font></b><br>",
                    "<br>",
                    "<b><u>Win probabilities</u></b><br>",
                    "<font color = 'red'><b>", LPC_cand, "</b> (LPC)</font>: ", round(100*LPC_prob), "%<br>",
                    "<font color = 'blue'><b>", CPC_cand, "</b> (CPC)</font>: ", round(100*CPC_prob), "%<br>",
                    "<font color = '#EE7600'><b>", NDP_cand, "</b> (NDP)</font>: ", round(100*NDP_prob), "%<br>",
                    "<font color = '#008B00'><b>", Green_cand, "</b> (Green)</font>: ", round(100*Green_prob), "%<br>",
                    "<br>",
                    "<b><u>Projected vote (90% CI)</u></b><br>",
                    "<font color = 'red'><b>LPC</b></font>: ", round(100*LPC_pred, 1), "% (", round(100*pct_05.LPC, 1), "%–", round(100*pct_95.LPC, 1), "%)<br>",
                    "<font color = 'blue'><b>CPC</b></font>: ", round(100*CPC_pred, 1), "% (", round(100*pct_05.CPC, 1), "%–", round(100*pct_95.CPC, 1), "%)<br>",
                    "<font color = '#EE7600'><b>NDP</b></font>: ", round(100*NDP_pred, 1), "% (", round(100*pct_05.NDP, 1), "%–", round(100*pct_95.NDP, 1), "%)<br>",
                    "<font color = '#008B00'><b>Green</b></font>: ", round(100*Green_pred, 1), "% (", round(100*pct_05.Green, 1), "%–", round(100*pct_95.Green, 1), "%)"
             ),
           
           ## Holds with independents
           last_winner %in% c("Independent", "CCF") & predicted_winner == "Independent" & incumbent_running ~
             paste0("<b><u><font size = 2.5>", name_english, "</font></b></u><br>",
                    "<b><font color = '#444444'>", last_winner, "</font> hold</b><br>",
                    "<br>",
                    "<b><u>Win probabilities</u></b><br>",
                    "<font color = 'red'><b>", LPC_cand, "</b> (LPC)</font>: ", round(100*LPC_prob), "%<br>",
                    "<font color = 'blue'><b>", CPC_cand, "</b> (CPC)</font>: ", round(100*CPC_prob), "%<br>",
                    "<font color = '#EE7600'><b>", NDP_cand, "</b> (NDP)</font>: ", round(100*NDP_prob), "%<br>",
                    "<font color = '#008B00'><b>", Green_cand, "</b> (Green)</font>: ", round(100*Green_prob), "%<br>",
                    "<font color = '#444444'><b>", ind_cand, "</b> (Ind)</font>: ", round(100*ind_prob), "%<br>",
                    "<br>",
                    "<b><u>Projected vote (90% CI)</u></b><br>",
                    "<font color = 'red'><b>LPC</b></font>: ", round(100*LPC_pred, 1), "% (", round(100*pct_05.LPC, 1), "%–", round(100*pct_95.LPC, 1), "%)<br>",
                    "<font color = 'blue'><b>CPC</b></font>: ", round(100*CPC_pred, 1), "% (", round(100*pct_05.CPC, 1), "%–", round(100*pct_95.CPC, 1), "%)<br>",
                    "<font color = '#EE7600'><b>NDP</b></font>: ", round(100*NDP_pred, 1), "% (", round(100*pct_05.NDP, 1), "%–", round(100*pct_95.NDP, 1), "%)<br>",
                    "<font color = '#008B00'><b>Green</b></font>: ", round(100*Green_pred, 1), "% (", round(100*pct_05.Green, 1), "%–", round(100*pct_95.Green, 1), "%)<br>",
                    "<font color = '#444444'><b>Ind</b></font>: ", round(100*ind_pred, 1), "% (", round(100*pct_05.ind, 1), "%–", round(100*pct_95.ind, 1), "%)"
              ),
           
           ## Flips from independents running
           last_winner %in% c("Independent", "CCF") & predicted_winner != "Independent" & incumbent_running ~
             paste0("<b><u><font size = 2.5>", name_english, "</font></b></u><br>",
                    "<b><font color = ", predicted_color, ">", predicted_winner, "</font> gain from <font color = '#444444'>", last_winner, "</font></b><br>",
                    "<br>",
                    "<b><u>Win probabilities</u></b><br>",
                    "<font color = 'red'><b>", LPC_cand, "</b> (LPC)</font>: ", round(100*LPC_prob), "%<br>",
                    "<font color = 'blue'><b>", CPC_cand, "</b> (CPC)</font>: ", round(100*CPC_prob), "%<br>",
                    "<font color = '#EE7600'><b>", NDP_cand, "</b> (NDP)</font>: ", round(100*NDP_prob), "%<br>",
                    "<font color = '#008B00'><b>", Green_cand, "</b> (Green)</font>: ", round(100*Green_prob), "%<br>",
                    "<font color = '#444444'><b>", ind_cand, "</b> (Ind)</font>: ", round(100*ind_prob), "%<br>",
                    "<br>",
                    "<b><u>Projected vote (90% CI)</u></b><br>",
                    "<font color = 'red'><b>LPC</b></font>: ", round(100*LPC_pred, 1), "% (", round(100*pct_05.LPC, 1), "%–", round(100*pct_95.LPC, 1), "%)<br>",
                    "<font color = 'blue'><b>CPC</b></font>: ", round(100*CPC_pred, 1), "% (", round(100*pct_05.CPC, 1), "%–", round(100*pct_95.CPC, 1), "%)<br>",
                    "<font color = '#EE7600'><b>NDP</b></font>: ", round(100*NDP_pred, 1), "% (", round(100*pct_05.NDP, 1), "%–", round(100*pct_95.NDP, 1), "%)<br>",
                    "<font color = '#008B00'><b>Green</b></font>: ", round(100*Green_pred, 1), "% (", round(100*pct_05.Green, 1), "%–", round(100*pct_95.Green, 1), "%)<br>",
                    "<font color = '#444444'><b>Ind</b></font>: ", round(100*ind_pred, 1), "% (", round(100*pct_05.ind, 1), "%–", round(100*pct_95.ind, 1), "%)"
             ),
           
           ## Flips from independents not running
           last_winner %in% c("Independent", "CCF") & predicted_winner != "Independent" & !incumbent_running ~
             paste0("<b><u><font size = 2.5>", name_english, "</font></b></u><br>",
                    "<b><font color = ", predicted_color, ">", predicted_winner, "</font> gain from <font color = '#444444'>", last_winner, "</font></b><br>",
                    "<br>",
                    "<b><u>Win probabilities</u></b><br>",
                    "<font color = 'red'><b>", LPC_cand, "</b> (LPC)</font>: ", round(100*LPC_prob), "%<br>",
                    "<font color = 'blue'><b>", CPC_cand, "</b> (CPC)</font>: ", round(100*CPC_prob), "%<br>",
                    "<font color = '#EE7600'><b>", NDP_cand, "</b> (NDP)</font>: ", round(100*NDP_prob), "%<br>",
                    "<font color = '#008B00'><b>", Green_cand, "</b> (Green)</font>: ", round(100*Green_prob), "%<br>",
                    "<br>",
                    "<b><u>Projected vote (90% CI)</u></b><br>",
                    "<font color = 'red'><b>LPC</b></font>: ", round(100*LPC_pred, 1), "% (", round(100*pct_05.LPC, 1), "%–", round(100*pct_95.LPC, 1), "%)<br>",
                    "<font color = 'blue'><b>CPC</b></font>: ", round(100*CPC_pred, 1), "% (", round(100*pct_05.CPC, 1), "%–", round(100*pct_95.CPC, 1), "%)<br>",
                    "<font color = '#EE7600'><b>NDP</b></font>: ", round(100*NDP_pred, 1), "% (", round(100*pct_05.NDP, 1), "%–", round(100*pct_95.NDP, 1), "%)<br>",
                    "<font color = '#008B00'><b>Green</b></font>: ", round(100*Green_pred, 1), "% (", round(100*pct_05.Green, 1), "%–", round(100*pct_95.Green, 1), "%)"
             )
           )
  ) %>%
  st_as_sf() %>%
  mutate(area = st_area(.)) %>%
  group_by(FED_NUM) %>%
  arrange(FED_NUM, desc(area)) %>%
  mutate(area_order = 1:n()) %>%
  filter((area_order == 1 & name_english != "Halifax") | (area_order == 2 & name_english == "Halifax")) %>%
  ungroup()

## Add "centroids"
coords_df <- st_coordinates(canada_districts_latlong) %>%
  as.data.frame() %>%
  as.tbl() %>%
  group_by(L2) %>%
  summarise(min_lng = min(X), max_lng = max(X), min_lat = min(Y), max_lat = max(Y)) %>%
  mutate(lng = (min_lng + max_lng)/2, 
         lat = (min_lat + max_lat)/2,
         FED_NUM = canada_districts_latlong$FED_NUM) %>%
  dplyr::select(FED_NUM, lng, lat)

## Mess with frigid northlands a bit because Mercator
coords_df$lat[338] <- coords_df$lat[337] <- coords_df$lat[336]

canada_districts_latlong <- canada_districts_latlong %>%
  left_join(coords_df, by = "FED_NUM")

## Remove large intermediate arrays generated during simulation
rm(list = grep("_rho.mat|_district_poll.mat", ls(), value = TRUE))

write_rds(canada_districts_latlong, "Shiny-app/canada_districts.rds")

canada_flips <- canada_districts_latlong %>%
  filter(predicted_winner != last_winner)

## Leaflet map
leaflet() %>%
  addPolygons(data = canada_districts_latlong, weight = 1, color = "#666666", opacity = 1, fillColor = ~fill_color, 
              fillOpacity = ~((pmax(2*max_prob - 1, 0.01))^0.5)/1.1, label = ~name_english, popup = ~district_info,
              highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)) %>%
  addPolylines(data = canada_flips, weight = 2, color = "black", opacity = 1, fillOpacity = 0, dashArray = "3") %>%
  addTiles(options = tileOptions(opacity = 0.4, fillOpacity = 0.75)) %>%
  setView(lng = -96.5, lat = 55, zoom = 4)
