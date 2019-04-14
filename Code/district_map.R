source("Code/library.R")

## Create Lambert conformal conic CRS for leaflet (see http://spatialreference.org/ref/esri/canada-lambert-conformal-conic/ )
crs_proj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
crs_lcc <- leafletCRS(code = "ESRI:102002", proj4def = crs_proj)

## Read shapefile
canada_districts <- readOGR(dsn = "Data/Shapefiles", layer = "FED_CA_2_2_ENG") %>%
  ms_simplify()


## Transform to lat-long
canada_districts_latlong <- spTransform(canada_districts, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  st_as_sf() %>%
  merge(district_key_2013, by.x = "FED_NUM", by.y = "district_code", all = FALSE)  %>%
  merge(LPC_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(CPC_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(NDP_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(Bloc_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(Green_distribution %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(district_probs %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(read_csv("Data/incumbents.csv") %>% dplyr::select(-name_english), by.x = "FED_NUM", by.y = "district_code", all.x = TRUE) %>%
  merge(cands_2019, by.x = "FED_NUM", by.y = "district_code") %>%
  mutate(max_prob = pmax(LPC_prob, CPC_prob, NDP_prob, Bloc_prob, Green_prob),
         predicted_winner = case_when(LPC_prob == max_prob ~ "Liberal",
                                      CPC_prob == max_prob ~ "Conservative",
                                      NDP_prob == max_prob ~ "NDP",
                                      Bloc_prob == max_prob ~ "Bloc",
                                      Green_prob == max_prob ~ "Green"),
         hold = (predicted_winner == last_winner),
         fill_color = case_when(LPC_prob == max_prob ~ "red",
                                CPC_prob == max_prob ~ "blue",
                                NDP_prob == max_prob ~ "#EE7600",
                                Bloc_prob == max_prob ~ "#8B008B",
                                Green_prob == max_prob ~ "#008B00"),
         district_info = case_when(
           PROVCODE == "QC" & predicted_winner == "Liberal" & last_winner == "Liberal" ~
             paste0("<b><u>", name_english, "</b></u><br>",
                    "<b><font color = 'red'>Liberal</font> hold</b><br>",
                    "<br>",
                    "<b><u>Win probabilities</u></b><br>",
                    "<font color = 'red'><b>", LPC_cand, "</b> (LPC)</font>: ", round(100*LPC_win_prob), "%<br>",
                    "<font color = 'blue'><b>", CPC_cand, "</b> (CPC)</font>: ", round(100*CPC_win_prob), "%<br>",
                    "<font color = '#EE7600'><b>", NDP_cand, "</b> (NDP)</font>: ", round(100*NDP_win_prob), "%<br>",
                    "<font color = '#8B008B'><b>", Bloc_cand, "</b> (Bloc)</font>: ", round(100*Bloc_win_prob), "%<br>",
                    "<font color = '#008B00'><b>", Green_cand, "</b> (Green)</font>: ", round(100*Green_win_prob), "%<br>",
                    "<br>",
                    "<b><u>Projected vote (90% CI)</u></b><br>",
                    "<font color = 'red'><b>LPC</b></font>: ", round(100*pct_50.LPC, 1), "% (", round(100*pct_05.LPC, 1), "%–", round(100*pct_95.LPC, 1), "%)<br>",
                    "<font color = 'blue'><b>CPC</b></font>: ", round(100*pct_50.CPC, 1), "% (", round(100*pct_05.CPC, 1), "%–", round(100*pct_95.CPC, 1), "%)<br>",
                    "<font color = '#EE7600'><b>NDP</b></font>: ", round(100*pct_50.NDP, 1), "% (", round(100*pct_05.NDP, 1), "%–", round(100*pct_95.NDP, 1), "%)<br>",
                    "<font color = '#8B008B'><b>Bloc</b></font>: ", round(100*pct_50.Bloc, 1), "% (", round(100*pct_05.Bloc, 1), "%–", round(100*pct_95.Bloc, 1), "%)<br>",
                    "<font color = '#008B00'><b>Green</b></font>: ", round(100*pct_50.Green, 1), "% (", round(100*pct_05.Green, 1), "%–", round(100*pct_95.Green, 1), "%)")
         )
  )
  
## Leaflet map
leaflet(canada_districts_latlong) %>%
  addPolygons(weight = 1, color = "#666666", opacity = 1, fillColor = ~fill_color, fillOpacity = ~max_prob^1.5, label = ~name_english,
              popup = ~district_info)
  
