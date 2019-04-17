library(lubridate)
library(reshape2)
library(tidyverse)
library(shiny)
library(shinythemes)
library(sp)
library(sf)
library(leaflet)

## Read in things
outcome_probs <- read_rds("outcome_probs.rds")
national_polls <- read_rds("national_polls.rds")
canada_districts_latlong <- read_rds("canada_districts.rds")
canada_flips <- canada_districts_latlong %>%
  filter(predicted_winner != last_winner)

## Create vectors for ridings and provinces
provinces <- canada_districts_latlong$province
province_names <- unique(provinces)
districts <- canada_districts_latlong$name_english

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
  titlePanel(paste0("Forecast as of ", month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today()))),
  sidebarLayout(
    
    ## Main panel: can display map or graphs
    mainPanel = mainPanel(leafletOutput("forecastmap", height = 900, width = "100%")),
    
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
                                                      selectInput(inputId = "riding_select", label = "", 
                                                           choices = c("Choose a riding", districts[provinces == "Newfoundland and Labrador"]))),
                                     conditionalPanel(condition = "input.province_select == 'Prince Edward Island'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "Prince Edward Island"]))),
                                     conditionalPanel(condition = "input.province_select == 'Nova Scotia'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "Nova Scotia"]))),
                                     conditionalPanel(condition = "input.province_select == 'New Brunswick'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "New Brunswick"]))),
                                     conditionalPanel(condition = "input.province_select == 'Quebec'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "Quebec"]))),
                                     conditionalPanel(condition = "input.province_select == 'Ontario'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "Ontario"]))),
                                     conditionalPanel(condition = "input.province_select == 'Manitoba'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "Manitoba"]))),
                                     conditionalPanel(condition = "input.province_select == 'Saskatchewan'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "Saskatchewan"]))),
                                     conditionalPanel(condition = "input.province_select == 'Alberta'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "Alberta"]))),
                                     conditionalPanel(condition = "input.province_select == 'British Columbia'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "British Columbia"]))),
                                     conditionalPanel(condition = "input.province_select == 'Yukon'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "Yukon"]))),
                                     conditionalPanel(condition = "input.province_select == 'Northwest Territories'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "Northwest Territories"]))),
                                     conditionalPanel(condition = "input.province_select == 'Nunavut'",
                                                      selectInput(inputId = "riding_select", label = "", 
                                                                  choices = c("Choose a riding", districts[provinces == "Nunavut"])
                                                                  )
                                     ),
                                     tags$head(tags$style(HTML(".selectize-input {width: 400px;}")))
                              )
                            )
                 ),
                 width = 3.01),
    position = "right"
    )
)

# Server
server <- function(input, output) {
  ## Forecast map
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
      setView(lng = -96.5, lat = 55, zoom = 4)
  })
  
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
  
  observe({
    if(input$riding_select %in% canada_districts_latlong$name_english) {
      leafletProxy('forecastmap') %>%
        setView(lng = center()$lng, lat = center()$lat, zoom = 7)
    }
    
  })
  
    ## National polls over time
    output$national_polls <- renderPlot({
      national_polls %>%
        reshape2::melt(measure.vars = c("LPC", "CPC", "NDP", "GPC", "PPC"),
                       variable.name = "Party", value.name = "Poll") %>%
        ggplot(aes(x = date, y = Poll, col = Party)) +
        geom_point(aes(size = sqrt(loess_weight)), alpha = 0.4) +
        geom_smooth(method = "loess", span = 0.25, size = 1) +
        scale_colour_manual(name = "Party", values = c("red", "blue", "darkorange1", "green4", "midnightblue"),
                            labels = c("Liberal", "Conservative", "NDP", "Green", "People's")) +
        scale_size_continuous(name = "Weight", range = c(0.1, 2)) +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
        theme(axis.text.x = element_text(angle = 90, size = 7)) +
        scale_y_continuous(breaks = 10*(0:6)) +
        labs(title = "2019 Canadian federal election polling",
             subtitle = "National", x = "Date", y = "%")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

