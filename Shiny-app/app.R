library(lubridate)
library(reshape2)
library(ggiraph)
library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(shinythemes)
library(sf)
library(leaflet)

## Read in things
outcome_probs <- read_rds("outcome_probs.rds")
national_polls_unadjusted <- read_rds("national_polls.rds") %>%
  mutate(party_abbr = case_when(Party == "Liberal" ~ "LPC",
                                Party == "Conservative" ~ "CPC",
                                Party == "NDP" ~ "NDP",
                                Party == "Green" ~ "Green",
                                Party == "Bloc" ~ "Bloc",
                                Party == "People's" ~ "PPC"),
         description = paste0(pollster, ", ", date, "\n n = ", n, "\n", party_abbr, ": ", round(Poll, 1), "%"))
national_polls_adjusted <- read_rds("national_polls_adjusted.rds") %>%
  mutate(party_abbr = case_when(Party == "Liberal" ~ "LPC",
                                Party == "Conservative" ~ "CPC",
                                Party == "NDP" ~ "NDP",
                                Party == "Green" ~ "Green",
                                Party == "Bloc" ~ "Bloc",
                                Party == "People's" ~ "PPC"),
         description = paste0(pollster, ", ", date, "\n n = ", n, "\n", party_abbr, ": ", round(Poll, 1), "%"))
poll_sds_unadjusted <- read_csv("poll_sds_unadjusted.csv")
poll_sds_adjusted <- read_csv("poll_sds_adjusted.csv")
poll_averages_unadjusted <- read_csv("poll_averages_unadjusted.csv") %>%
  mutate(lower = pct - 1.644*poll_sds_unadjusted$sd,
         upper = pct + 1.644*poll_sds_unadjusted$sd,
         Party = ordered(Party, levels = c("Liberal", "Conservative", "NDP", "Green", "People's"))) %>%
  mutate(description = case_when(Party == "Liberal" ~ paste0("LPC average: ", round(pct, 1), "% (", round(lower, 1), "-", round(upper, 1), "%)"),
                                 Party == "Conservative" ~ paste0("CPC average: ", round(pct, 1), "% (", round(lower, 1), "-", round(upper, 1), "%)"),
                                 Party == "NDP" ~ paste0("NDP average: ", round(pct, 1), "% (", round(lower, 1), "-", round(upper, 1), "%)"),
                                 Party == "Green" ~ paste0("Green average: ", round(pct, 1), "% (", round(lower, 1), "-", round(upper, 1), "%)"),
                                 Party == "People's" ~ paste0("PPC average: ", round(pct, 1), "% (", round(lower, 1), "-", round(upper, 1), "%)")),
         description = paste0(format(date, "%B %e, %Y"), "\n", description)
  ) %>%
  arrange(Party, date)

poll_averages_adjusted <- read_csv("poll_averages_adjusted.csv")  %>%
  mutate(lower = pct - 1.644*poll_sds_adjusted$sd,
         upper = pct + 1.644*poll_sds_adjusted$sd,
         Party = ordered(Party, levels = c("Liberal", "Conservative", "NDP", "Green", "People's"))) %>%
  mutate(description = case_when(Party == "Liberal" ~ paste0("LPC average: ", round(pct, 1), "% (", round(lower, 1), "-", round(upper, 1), "%)"),
                                 Party == "Conservative" ~ paste0("CPC average: ", round(pct, 1), "% (", round(lower, 1), "-", round(upper, 1), "%)"),
                                 Party == "NDP" ~ paste0("NDP average: ", round(pct, 1), "% (", round(lower, 1), "-", round(upper, 1), "%)"),
                                 Party == "Green" ~ paste0("Green average: ", round(pct, 1), "% (", round(lower, 1), "-", round(upper, 1), "%)"),
                                 Party == "People's" ~ paste0("PPC average: ", round(pct, 1), "% (", round(lower, 1), "-", round(upper, 1), "%)")),
         description = paste0(format(date, "%B %e, %Y"), "\n", description)
  ) %>%
  arrange(Party, date)

forecast_timeline <- read_csv("forecast_timeline.csv") %>%
  mutate(description = paste0(format(date, "%B %e, %Y"), "\n",
                              outcome, ": ", round(100*prob), "%"))

canada_districts_latlong <- read_rds("canada_districts.rds") %>%
  merge(read_csv("district_zoom_levels_2013.csv"), by.x = "FED_NUM", by.y = "district_code") 

canada_flips <- canada_districts_latlong %>%
  filter(predicted_winner != last_winner)

data_2019 <- read_rds("data_2019.rds")
models_list <- read_rds("models.rds")

seat_simulations <- read_rds("seat_simulations.rds") %>%
  melt(id.vars = c("simulation", "most_seats", "type_of_win"), variable.name = "party", value.name = "seats") %>%
  as.tbl() %>%
  mutate(round_seats = 5*floor(seats/5))

BPI <- read_rds("bpi.rds")

## Define waterfall functions
source("waterfall.R")

## Create vectors for ridings and provinces
provinces <- canada_districts_latlong %>% 
  as.data.frame() %>% 
  dplyr::select(name_english, province) %>% 
  distinct(.keep_all = TRUE) %>% 
  pull(province)

province_names <- unique(provinces)

districts <- canada_districts_latlong %>% 
  as.data.frame() %>% 
  dplyr::select(name_english, province) %>% 
  distinct(.keep_all = TRUE) %>% 
  pull(name_english)

district_codes <- canada_districts_latlong %>% 
  as.data.frame() %>% 
  dplyr::select(FED_NUM, province) %>% 
  distinct(.keep_all = TRUE) %>% 
  pull(FED_NUM)

names(district_codes) <- districts

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    ## Some light CSS for table formatting
    tags$style(HTML("
                    table {
                      border-spacing: 2px;
                    }
                    
                    th, td {
                      padding: 3px;
                    }
                    "))
    ), 
  navbarPage("The Election StatSheet Canada 2019 forecast",
    ## Map tab         
    tabPanel("Map",
        sidebarLayout(
        
        ## Main panel: can display map or graphs
        mainPanel = mainPanel(leafletOutput("forecastmap", height = 700, width = "100%"),
                              hr(),
                              conditionalPanel("input.forecastmap_shape_click", 
                                               ggiraphOutput("forecast_breakdown", width = "150%", height = "400px"))),
        
        ## Sidebar
        sidebarPanel(tags$h4("Current forecast"),
                     tags$h5("Outcome probabilities"),
                     HTML(paste0("<table>",
                                 "<tr>",
                                 "<th><u>Party</u></th><th><u>Majority</u></th><th><u>Minority</u></th>",
                                 "</tr>",
                                 "<tr>",
                                 "<td><b><font color = 'blue'>Conservative</font></b></td>", 
                                 "<td>", round(100*outcome_probs[1,2]), "%</td>",
                                 "<td>", round(100*outcome_probs[1,3]), "%</td>",
                                 "</tr>",
                                 "<tr>",
                                 "<td><b><font color = 'red'>Liberal</font></b></td>", 
                                 "<td>", round(100*outcome_probs[2,2]), "%</td>",
                                 "<td>", round(100*outcome_probs[2,3]), "%</td>",
                                 "</tr>",  
                                 "</table>")
                     ),
                     
                     ## Select province and show district menu
                     inputPanel(selectInput(inputId = "province_select", label = "Find a riding", choices = c("Choose a province or territory", provinces), 
                                            selected = "Choose a province or territory"),
                                tags$head(tags$style(HTML(".selectize-input {width: 400px;}"))),
                                uiOutput("riding_menu"),
                                tags$head(tags$style(HTML(".selectize-input {width: 400px;}"))),
                                conditionalPanel(condition = "input.riding_menu !== 'Choose a riding'",
                                                 actionButton("go_district", "Go!")),
                                tags$head(tags$style(HTML(".selectize-input {width: 400px;}"))),
                                selectInput("party_select", label = "Party", choices = c("Liberal", "Conservative", "NDP", "Green", "Bloc"))
                                ),
                                hr(),
                                hr(),
                                hr(),
                     width = 3.01),
        position = "right")
    ),
    
    ## Polls and timeline
    tabPanel("Graphs",
      sidebarLayout(
        ## Main panel: display graphs
        mainPanel = mainPanel(ggiraphOutput("forecastgraph")),
        
        ## Sidebar panel: choose between timeline of probabilities, national polls over time, and provincial polls over time
        sidebarPanel = sidebarPanel(tags$h3("Choose a graph"),
                                    radioButtons("graph_type",
                                                 label = "Graph",
                                                 choices = c("National polls",
                                                             "Forecast over time",
                                                             "Seat distributions",
                                                             "Bellwether-o-gram")
                                                 ),
                                    conditionalPanel(condition = "input.graph_type == 'National polls'",
                                                     sliderInput("date_range_polls", "Date range", min = as.Date("2015-10-19"), max = as.Date("2019-10-21"),
                                                                 value = as.Date(c("2015-10-19", "2019-10-21"))
                                                                 )
                                    ),
                                    conditionalPanel(condition = "input.graph_type == 'National polls'",
                                                     checkboxInput("house_adjust", "Adjust for pollster house effects?", value = TRUE)
                                    ),
                                    conditionalPanel(condition = "input.graph_type == 'Forecast over time'",
                                                     sliderInput("date_range_probs", "Date range", min = as.Date("2019-04-17"), max = as.Date("2019-10-21"),
                                                                 value = as.Date(c("2019-04-17", "2019-10-21")))),
                                    conditionalPanel(condition = "input.graph_type == 'Seat distributions'",
                                                     checkboxGroupInput("histogram_parties", "Parties to display",
                                                                        choices = c("Liberal", "Conservative", "NDP", "Bloc", "Green"),
                                                                        selected = c("Liberal", "Conservative", "NDP", "Bloc")))
                                    ),
        position = "right")
      )
    )
)

# Server
server <- function(input, output) {
  
  # Ridings available given province
  output$riding_menu <- renderUI({
    selectInput("riding_select", label = "Riding", choices = c("Choose a riding", district_codes[provinces == input$province_select]))
  })

  # Forecast map
  output$forecastmap <- renderLeaflet({
    leaflet() %>%
      addMapPane(name = "worldmap", zIndex = 400) %>%
      addMapPane(name = "polygons", zIndex = 420) %>%
      addMapPane(name = "borders", zIndex = 440) %>%
      addPolygons(data = canada_districts_latlong, layerId = ~FED_NUM, weight = 1, color = "#666666", opacity = 1, fillColor = ~fill_color, 
                  fillOpacity = ~((pmax(2*max_prob - 1, 0.01))^0.5)/1.15, label = ~name_english, popup = ~district_info,
                  highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE),
                  options = leafletOptions(pane = "polygons")) %>%
      addPolylines(data = canada_flips, weight = 2, color = "black", opacity = 1, fillOpacity = 0, dashArray = "3",
                   options = leafletOptions(pane = "borders")) %>%
      addTiles(options = tileOptions(opacity = 1, fillOpacity = 1, pane = "worldmap")) %>%
      setView(lng = -96.5, lat = 53, zoom = 4)
  })
  
  # Reacting to user choices of districts
  
  ## Center on district
  center <- reactive({
    return(canada_districts_latlong %>% 
             group_by(name_english) %>%
             arrange(name_english, lng) %>%
             mutate(n = 1:n()) %>%
             ungroup() %>%
             filter(n == 1,
                    FED_NUM == input$riding_select) %>%
             st_as_sf()
    )
  })
  
  ## The event in question: the click of the Go button (input$go_district)
  observeEvent(
    input$go_district,
    handlerExpr = {
      leafletProxy("forecastmap") %>%
        setView(lng = center()$lng, lat = center()$lat, zoom = center()$zoom_level)
      }
    )
  
  ## Forecast breakdown
  forecastBreakdown <- eventReactive( # react to either a click on the map or a district selection
    {input$forecastmap_shape_click
      input$party_select},
    valueExpr = {
      girafe(ggobj = make_waterfall_plot(make_waterfall_data(as.numeric(input$forecastmap_shape_click$id), input$party_select)),
             pointsize = 16, width_svg = 12, height_svg = 4)
    })
  
  output$forecast_breakdown <- renderggiraph({
    forecastBreakdown()
  })
  
  ## Graphs
  output$forecastgraph <- renderggiraph({
    if(input$graph_type == "National polls" & input$house_adjust) {
      girafe(ggobj = (national_polls_adjusted %>%
        ggplot(aes(x = date, y = Poll, col = Party)) +
        geom_ribbon(data = poll_averages_adjusted, aes(y = NULL, ymin = lower, ymax = upper, col = NA, fill = Party), 
                    alpha = 0.2, show.legend = FALSE) +
        geom_vline(xintercept = as.Date("2019-10-21")) +
        geom_point_interactive(aes(size = sqrt(loess_weight), tooltip = description), alpha = 0.2) +
        geom_point_interactive(data = poll_averages_adjusted, aes(x = date, y = pct, col = Party, tooltip = description), size = 0.5) +
        geom_line(data = poll_averages_adjusted, aes(x = date, y = pct, group = Party, col = Party)) +
        scale_colour_manual(name = "Party", values = c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Green" = "green4", 
                                                       "People's" = "midnightblue")) +
        scale_fill_manual(name = "Party", values = c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Green" = "green4", 
                                                     "People's" = "midnightblue")) +
        scale_size_continuous(name = "Weight", range = c(0.1, 2)) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", limits = input$date_range_polls) +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        scale_y_continuous(breaks = 10*(0:6)) +
        labs(title = "2019 Canadian federal election polling", subtitle = "Adjusted for house effects", x = "Date", y = "% of vote")),
        width_svg = 11
      )
      
    } else if(input$graph_type == "National polls" & !input$house_adjust) {
      girafe(ggobj = (national_polls_unadjusted %>%
        ggplot(aes(x = date, y = Poll, col = Party)) +
        geom_ribbon(data = poll_averages_unadjusted, aes(y = NULL, ymin = lower, ymax = upper, col = NA, fill = Party), 
                    alpha = 0.2, show.legend = FALSE) +
        geom_vline(xintercept = as.Date("2019-10-21")) +
        geom_point_interactive(aes(size = sqrt(loess_weight), tooltip = description), alpha = 0.2) +
        geom_point_interactive(data = poll_averages_unadjusted, aes(x = date, y = pct, col = Party, tooltip = description), size = 0.5) +
        geom_line(data = poll_averages_unadjusted, aes(x = date, y = pct, group = Party, col = Party)) +
        scale_colour_manual(name = "Party", values = c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Green" = "green4", 
                                                       "People's" = "midnightblue")) +
        scale_fill_manual(name = "Party", values = c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Green" = "green4", 
                                                     "People's" = "midnightblue")) +
        scale_size_continuous(name = "Weight", range = c(0.1, 2)) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", limits = input$date_range_polls) +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        scale_y_continuous(breaks = 10*(0:6)) +
        labs(title = "2019 Canadian federal election polling", subtitle = "Unadjusted for house effects", x = "Date", y = "% of vote")),
        width_svg = 11
      )
      
      } else if(input$graph_type == "Forecast over time") {
        girafe(ggobj = (forecast_timeline %>%
        ggplot() +
        geom_point_interactive(aes(x = date, y = prob, group = outcome, col = outcome, tooltip = description), size = 1) +
        geom_line(aes(x = date, y = prob, group = outcome, col = outcome)) +
        geom_vline(xintercept = as.Date("2019-10-21")) +
        scale_colour_manual(name = "Outcome", values = c("blue", "#AAAAFF", "red", "#FFAAAA", "darkorange1", "#FFBB77"),
                            labels = c("Conservative majority", "Conservative minority", "Liberal majority", "Liberal minority", 
                                       "NDP majority", "NDP minority")) +
        scale_x_date(breaks = "2 weeks", limits = input$date_range_probs) +
        scale_y_continuous(breaks = 0.05*(0:10), limits = c(0, 0.5)) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = "Forecast over time", subtitle = "How has the forecast changed?", x = "Date", y = "Probability")),
        width_svg = 11
        )
      } else if(input$graph_type == "Seat distributions") {
        girafe(ggobj = (
          seat_simulations %>%
            group_by(party) %>%
            mutate(n = n()) %>%
            group_by(party, round_seats) %>%
            summarise(prob = n()/mean(n)) %>%
            ungroup() %>%
            mutate(party = case_when(party == "LPC" ~ "Liberal",
                                     party == "CPC" ~ "Conservative",
                                     party == "NDP" ~ "NDP",
                                     party == "Green" ~ "Green",
                                     party == "Bloc" ~ "Bloc"),
                   description = enc2utf8(paste0(party, ", ", round_seats, "–", round_seats + 4, " seats\nProbability: ", round(100*prob, 1), "%"))) %>%
            filter(party %in% input$histogram_parties) %>%
            ggplot(aes(x = round_seats, y = prob, fill = party)) + 
            geom_bar_interactive(aes(tooltip = description), stat = "identity", position = "identity", alpha = 0.5, col = "black") +
            geom_vline(xintercept = 170, size = 1) +
            geom_text(x = 195, y = 0.1, label = "170 seats needed\nfor a majority") +
            scale_y_continuous(breaks = 0.05*(0:20)) +
            scale_fill_manual(name = "Party", values = c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Bloc" = "#8ECEF9",
                                                         "Green" = "green4")) +
            labs(title = "Seat distribution by party", subtitle = "How many seats will each party win?", x = "Seats", y = "Probability")),
          width_svg = 11
        )
      } else if(input$graph_type == "Bellwether-o-gram") {
        girafe(ggobj = (
          ggplot(BPI %>% filter(!grepl("\\'", description)), aes(x = LPC, y = CPC, col = LPC_rel_logit)) +
            geom_text_interactive(aes(label = name_english, tooltip = description), size = 3) +
            scale_colour_gradient(low = "blue", high = "red", name = "LPC relative logit") +
            labs(title = "Bellwether-o-gram", x = "P(LPC government|LPC wins district)", y = "P(CPC government|CPC wins district)",
                 subtitle = "What's the probability that the Liberals/Conservatives win the election given that they win district ___?")),
          width_svg = 11)
      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)