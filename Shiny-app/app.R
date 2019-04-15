library(lubridate)
library(tidyverse)
library(shiny)
library(shinythemes)
library(sp)
library(sf)
library(leaflet)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
                    table {
                      border-spacing: 3px;
                    }
                    
                    th, td {
                      padding: 5px;
                    }
                    "))
    ), 
  titlePanel(HTML(paste0("<b>Forecast as of ", month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today()))), "</b>"),
  sidebarLayout(
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
                 width = 2),
    position = "right"
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ## Read in shapefiles
  canada_districts_latlong <- read_rds("canada_districts.rds")
  canada_flips <- canada_districts_latlong %>%
    filter(predicted_winner != last_winner)
  
  ## Read in forecast outputs
  outcome_probs <- read_rds("outcome_probs.rds")
  
  output$forecastmap <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = canada_districts_latlong, weight = 1, color = "#666666", opacity = 1, fillColor = ~fill_color, 
                  fillOpacity = ~((pmax(2*max_prob - 1, 0.01))^0.5)/1.1, label = ~name_english, popup = ~district_info,
                  highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)) %>%
      addPolylines(data = canada_flips, weight = 2, color = "black", opacity = 1, fillOpacity = 0, dashArray = "3") %>%
      addTiles(options = tileOptions(opacity = 0.4, fillOpacity = 0.75)) %>%
      setView(lng = -96.5, lat = 55, zoom = 4)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

