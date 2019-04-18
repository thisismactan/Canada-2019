library(lubridate)
library(reshape2)
library(tidyverse)
library(readr)
library(shiny)
library(shinythemes)
library(sp)
library(sf)
library(leaflet)

## Read in things
outcome_probs <- read_rds("outcome_probs.rds")
national_polls_unadjusted <- read_rds("national_polls.rds")
national_polls_adjusted <- read_rds("national_polls_adjusted.rds")
poll_sds_unadjusted <- read_csv("poll_sds_unadjusted.csv")
poll_sds_adjusted <- read_csv("poll_sds_adjusted.csv")
poll_averages_unadjusted <- read_csv("poll_averages_unadjusted.csv") %>%
  mutate(lower = pct - 1.644*poll_sds_unadjusted$sd,
         upper = pct + 1.644*poll_sds_unadjusted$sd,
         Party = ordered(Party, levels = c("Liberal", "Conservative", "NDP", "Green", "People's")))

poll_averages_adjusted <- read_csv("poll_averages_adjusted.csv")  %>%
  mutate(lower = pct - 1.644*poll_sds_adjusted$sd,
         upper = pct + 1.644*poll_sds_adjusted$sd,
         Party = ordered(Party, levels = c("Liberal", "Conservative", "NDP", "Green", "People's")))

forecast_timeline <- read_csv("forecast_timeline.csv")
canada_districts_latlong <- read_rds("canada_districts.rds")
canada_flips <- canada_districts_latlong %>%
  filter(predicted_winner != last_winner)

data_2019 <- read_rds("data_2019.rds")
models_list <- read_rds("models.rds")

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

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    ## Some light CSS for table formatting
    tags$style(HTML("
                    table {
                      border-spacing: 3px;
                    }
                    
                    th, td {
                      padding: 5px;
                    }
                    "))
    ), 
  navbarPage("The Election StatSheet Canada 2019 forecast",
    ## Map tab         
    tabPanel("Map",
      titlePanel(paste0("Forecast as of ", month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today()))),
      sidebarLayout(
        
        ## Main panel: can display map or graphs
        mainPanel = mainPanel(leafletOutput("forecastmap", height = 775, width = "100%")),
        
        ## Sidebar
        sidebarPanel(tags$h3("Current forecast"),
                     tags$h4("Outcome probabilities"),
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
                     inputPanel(selectInput(inputId = "province_select", label = "Go to a riding", choices = c("Choose a province or territory", provinces), 
                                            selected = "Choose a province or territory"),
                                tags$head(tags$style(HTML(".selectize-input {width: 400px;}"))),
                                fluidRow(
                                  column(1,
                                         conditionalPanel(condition = "input.province_select == 'Newfoundland and Labrador'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "Newfoundland and Labrador"]))),
                                         conditionalPanel(condition = "input.province_select == 'Prince Edward Island'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "Prince Edward Island"]))),
                                         conditionalPanel(condition = "input.province_select == 'Nova Scotia'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "Nova Scotia"]))),
                                         conditionalPanel(condition = "input.province_select == 'New Brunswick'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "New Brunswick"]))),
                                         conditionalPanel(condition = "input.province_select == 'Quebec'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "Quebec"]))),
                                         conditionalPanel(condition = "input.province_select == 'Ontario'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "Ontario"]))),
                                         conditionalPanel(condition = "input.province_select == 'Manitoba'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "Manitoba"]))),
                                         conditionalPanel(condition = "input.province_select == 'Saskatchewan'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "Saskatchewan"]))),
                                         conditionalPanel(condition = "input.province_select == 'Alberta'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "Alberta"]))),
                                         conditionalPanel(condition = "input.province_select == 'British Columbia'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "British Columbia"]))),
                                         conditionalPanel(condition = "input.province_select == 'Yukon'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "Yukon"]))),
                                         conditionalPanel(condition = "input.province_select == 'Northwest Territories'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "Northwest Territories"]))),
                                         conditionalPanel(condition = "input.province_select == 'Nunavut'",
                                                          selectInput(inputId = "riding_select", label = "Riding", 
                                                                      choices = c("Choose a riding", districts[provinces == "Nunavut"])
                                                          )
                                         ),
                                         conditionalPanel(condition = "input.riding_select != 'Choose a riding' & input.riding_select != 'Quebec'",
                                                          selectInput(inputId = "party_select", label = "Party",
                                                                      choices = c("Choose a party", "Liberal", "Conservative", "NDP", "Green"),
                                                                      width = "400px")),
                                         conditionalPanel(condition = "input.riding_select == 'Quebec'",
                                                          selectInput(inputId = "party_select", label = "Party",
                                                                      choices = c("Choose a party", "Liberal", "Conservative", "NDP", "Green", "Bloc"),
                                                                      width = "400px")),
                                         conditionalPanel(condition = "input.party_select != 'Choose a party'",
                                                          actionButton("go_district", "Go!")),
                                         plotOutput("forecast_breakdown", width = "600px", height = "280px")
                                  )
                                )
                     ),
                     width = 3.01),
        position = "right"
      )
    ),
    
    ## Polls and timeline
    tabPanel("Graphs",
      sidebarLayout(
        ## Main panel: display graphs
        mainPanel = mainPanel(plotOutput("forecastgraph", height = 750, width = "100%")),
        
        ## Sidebar panel: choose between timeline of probabilities, national polls over time, and provincial polls over time
        sidebarPanel = sidebarPanel(tags$h3("Choose a graph"),
                                    radioButtons("graph_type",
                                                 label = "Graph",
                                                 choices = c("National polls",
                                                             "Forecast over time")
                                                 ),
                                    conditionalPanel(condition = "input.graph_type == 'National polls'",
                                                     sliderInput("date_range_polls", "Date range", min = as.Date("2015-10-19"), max = as.Date("2019-10-21"),
                                                                 value = as.Date(c("2015-10-19", "2019-10-21"))
                                                                 )
                                    ),
                                    conditionalPanel(condition = "input.graph_type == 'National polls'",
                                                     checkboxInput("house_adjust", "Adjust for pollster house effects?", value = T)
                                    ),
                                    conditionalPanel(condition = "input.graph_type == 'Forecast over time'",
                                                     sliderInput("date_range_probs", "Date range", min = as.Date("2019-04-17"), max = as.Date("2019-10-21"),
                                                                 value = as.Date(c("2019-04-17", "2019-10-21"))
                                                     )
                                    )
                                    ),
        position = "right")
      )
    )
)

# Server
server <- function(input, output) {
  # Forecast map
  output$forecastmap <- renderLeaflet({
    leaflet() %>%
      addMapPane(name = "worldmap", zIndex = 400) %>%
      addMapPane(name = "polygons", zIndex = 420) %>%
      addMapPane(name = "borders", zIndex = 440) %>%
      addPolygons(data = canada_districts_latlong, weight = 1, color = "#666666", opacity = 1, fillColor = ~fill_color, 
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
             mutate(n = 1:n()) %>%
             ungroup() %>%
             filter(n == 1,
                    name_english == input$riding_select) %>%
             st_as_sf()
    )
  })
  
  ## The event in question: the click of the Go button (input$go_district)
  observeEvent(input$go_district,
               handlerExpr = {
                 leafletProxy('forecastmap') %>%
                   setView(lng = center()$lng, lat = center()$lat, zoom = 7)
                 })
  
  ## Forecast breakdown
  observeEvent(input$go_district,
               handlerExpr = {
                 output$forecast_breakdown <- renderPlot({
                   make_waterfall_plot(make_waterfall_data(input$riding_select, input$party_select))
                 })
               })
  
  ## Graphs
  output$forecastgraph <- renderPlot({
    if(input$graph_type == "National polls" & input$house_adjust) {
      national_polls_adjusted %>%
        reshape2::melt(measure.vars = c("LPC", "CPC", "NDP", "GPC", "PPC"),
                       variable.name = "Party", value.name = "Poll") %>%
        mutate(Party = case_when(Party == "LPC" ~ "Liberal",
                                 Party == "CPC" ~ "Conservative",
                                 Party == "NDP" ~ "NDP",
                                 Party == "GPC" ~ "Green",
                                 Party == "PPC" ~ "People's"),
               Party = ordered(Party, levels = c("Liberal", "Conservative", "NDP", "Green", "People's"))) %>%
        arrange(Party) %>%
        ggplot(aes(x = date, y = Poll, col = Party)) +
        geom_vline(xintercept = as.Date("2019-10-21")) +
        geom_point(aes(size = sqrt(loess_weight)), alpha = 0.4) +
        geom_ribbon(data = poll_averages_adjusted %>% arrange(Party), aes(y = NULL, ymin = lower, ymax = upper, col = NA, fill = Party), 
                    alpha = 1/3, show.legend = FALSE) +
        geom_line(data = poll_averages_unadjusted %>% arrange(Party), aes(y = pct, col = Party), size = 1) +
        scale_colour_manual(name = "Party", values = c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Green" = "green4", 
                                                       "People's" = "midnightblue")) +
        scale_fill_manual(name = "Party", values = c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Green" = "green4", 
                                                     "People's" = "midnightblue")) +
        scale_size_continuous(name = "Weight", range = c(0.1, 2)) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", limits = input$date_range_polls) +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        scale_y_continuous(breaks = 10*(0:6)) +
        labs(title = "2019 Canadian federal election polling", x = "Date", y = "% of vote")
      
    } else if(input$graph_type == "National polls" & !input$house_adjust) {
      national_polls_unadjusted %>%
        reshape2::melt(measure.vars = c("LPC", "CPC", "NDP", "GPC", "PPC"),
                       variable.name = "Party", value.name = "Poll") %>%
        mutate(Party = case_when(Party == "LPC" ~ "Liberal",
                                 Party == "CPC" ~ "Conservative",
                                 Party == "NDP" ~ "NDP",
                                 Party == "GPC" ~ "Green",
                                 Party == "PPC" ~ "People's"),
               Party = ordered(Party, levels = c("Liberal", "Conservative", "NDP", "Green", "People's"))) %>%
        arrange(Party) %>%
        ggplot(aes(x = date, y = Poll, col = Party)) +
        geom_vline(xintercept = as.Date("2019-10-21")) +
        geom_point(aes(size = sqrt(loess_weight)), alpha = 0.4) +
        geom_ribbon(data = poll_averages_unadjusted %>% arrange(Party), aes(y = NULL, ymin = lower, ymax = upper, col = NA, fill = Party), 
                    alpha = 1/3, show.legend = FALSE) +
        geom_line(data = poll_averages_unadjusted %>% arrange(Party), aes(y = pct, col = Party), size = 1) +
        scale_colour_manual(name = "Party", values = c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Green" = "green4", 
                                                       "People's" = "midnightblue")) +
        scale_fill_manual(name = "Party", values = c("Liberal" = "red", "Conservative" = "blue", "NDP" = "darkorange1", "Green" = "green4", 
                                                     "People's" = "midnightblue")) +
        scale_size_continuous(name = "Weight", range = c(0.1, 2)) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", limits = input$date_range_polls) +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        scale_y_continuous(breaks = 10*(0:6)) +
        labs(title = "2019 Canadian federal election polling", x = "Date", y = "% of vote")
      
      } else if(input$graph_type == "Forecast over time") {
        forecast_timeline %>%
        ggplot(aes(x = date, y = prob, col = outcome)) +
        geom_line(size = 1) +
        geom_vline(xintercept = as.Date("2019-10-21")) +
        scale_colour_manual(name = "Outcome", values = c("blue", "#AAAAFF", "red", "#FFAAAA", "darkorange1", "#FFBB77"),
                            labels = c("Conservative majority", "Conservative minority", "Liberal majority", "Liberal minority", 
                                       "NDP majority", "NDP minority")) +
        scale_x_date(breaks = "1 week", date_labels = "%b %e, %Y", limits = input$date_range_probs) +
        scale_y_continuous(breaks = 0.05*(0:10), limits = c(0, 0.5)) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = "Forecast over time", x = "Date", y = "Probability")
      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)