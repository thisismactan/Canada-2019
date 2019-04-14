library(lubridate)
library(tidyverse)
library(shiny)
library(sp)
library(sf)
library(leaflet)

ui <- fluidPage(
  titlePanel(paste0("Forecast as of ", month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today()))),
  sidebarPanel(strong("Current forecast:"), width = 2),
  leafletOutput("forecastmap", height = 800, width = "80%")
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

