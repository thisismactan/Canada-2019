library(lubridate)
library(reshape2)
library(tidyverse)
library(shiny)
library(shinythemes)
library(sp)
library(sf)
library(leaflet)

outcome_probs <- read_rds("outcome_probs.rds")
national_polls <- read_rds("national_polls.rds")
canada_districts_latlong <- read_rds("canada_districts.rds")
canada_flips <- canada_districts_latlong %>%
  filter(predicted_winner != last_winner)

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
  titlePanel(HTML(paste0("Forecast as of ", month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))),
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
                 radioButtons("panel_view", label = "",
                              choices = c("Map" = "forecastmap",
                                          "National polls" = "national_polls"),
                              selected = "forecastmap"),
                 width = 3),
    position = "right"
    )
)

# Server
server <- function(input, output) {
  ## Forecast map
  output$forecastmap <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = canada_districts_latlong, weight = 1, color = "#666666", opacity = 1, fillColor = ~fill_color, 
                  fillOpacity = ~((pmax(2*max_prob - 1, 0.01))^0.5)/1.1, label = ~name_english, popup = ~district_info,
                  highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)) %>%
      addPolylines(data = canada_flips, weight = 2, color = "black", opacity = 1, fillOpacity = 0, dashArray = "3") %>%
      addTiles(options = tileOptions(opacity = 0.4, fillOpacity = 0.75)) %>%
      setView(lng = -96.5, lat = 55, zoom = 4)
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

