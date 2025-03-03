library(tidyverse)
library(shiny)
library(DT)
library(leaflet)
library(bslib) #fonts
library(here)
library(sf)

#do data cleaning and r stuff above here
bird_data_top <- readRDS(here("bird_data_top.rds"))



# Count of each bird species observed in the dataset
species_counts <- bird_data_top |> 
  count(common_name, observation_date, name = "Total_Observations")

# Get unique species names for the select input
species_list <- unique(bird_data_top$common_name)

santa_barbara_geo <- read_sf(here("data", "ca_counties", "CA_Counties_TIGER2016.shp")) |> 
  filter(NAME == "Santa Barbara") |> 
  st_transform(4326)

### create the user interface

ui <- navbarPage(
  theme = bs_theme(bootswatch = 'minty'),
  "Santa Barbara County Birding App",
  
  tabPanel("Overview",
           h2("Welcome to the Santa Barbara County Birding App"),
           p("This app provides an interactive way to explore bird populations in Santa Barbara County over time."),
           h3("Key Features"),
           p("- View an interactive map of birding hotspots"),
           p("- Explore visualizations of bird population trends over time"),
           p("- Filter bird data based on species, year, and migration type"),
           h3("How to Use"),
           p("Navigate through the tabs to explore maps, graphs, and datasets. Start by selecting a species in the Visualization tab.")),
  
  tabPanel("Map",
           h2("Map of Mainland Santa Barbara County"),
           p("Explore bird observations in mainland Santa Barbara County."),
           selectInput("select", label = h4("Select Species"), choices = species_list, selected = species_list[1]),
           sliderInput("year", label = h4("Select Year:"), min = 1990, max = 2024, value = 2024, step = 1, animate = TRUE, sep = ""),
           leafletOutput("sb_map", height = "600px")),
  
  tabPanel("Visualization",
           h2("Species Count Over Time"),
           selectInput("select", label = h4("Select Species"), choices = species_list, selected = species_list[1]),
           plotOutput(outputId = "species_count_plot")),
  
  tabPanel("Data Table",
           h2("Bird Observation Dataset"),
           DTOutput("bird_data_table"))
)

### create the server function

server <- function(input, output) {
  filtered_data <- reactive({
    bird_data_top %>% 
      filter(common_name == input$select, year(observation_date) == input$year)
  })
  
  output$sb_map <- renderLeaflet({
    leaflet(data = santa_barbara_geo) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolygons(
        fillColor = "#74c476",
        color = "#006d2c",
        weight = 2,
        opacity = 1,
        fillOpacity = 0.6,
        label = ~NAME,
        highlightOptions = highlightOptions(
          color = "#ff7800",
          weight = 3,
          bringToFront = TRUE
        )
      ) %>%
      addCircleMarkers(
        data = filtered_data(),
        lng = ~longitude,
        lat = ~latitude,
        radius = 4,
        color = "red",
        fillOpacity = 0.7,
        popup = ~paste("Species:", common_name, "<br>Year:", year(observation_date))
      ) %>%
      setView(lng = -119.7, lat = 34.5, zoom = 9)  # Focus on mainland Santa Barbara
  })
  
  species_filtered <- reactive({
    species_counts %>% 
      filter(common_name == input$select)
  })
  
  output$species_count_plot <- renderPlot({
    ggplot(data = species_filtered(), aes(x = observation_date, y = Total_Observations)) +
      geom_line(color = "blue") +
      geom_point() +
      labs(title = paste("Observations Over Time for", input$select),
           x = "Year", y = "Total Observations") +
      theme_minimal()
  })
}

### combine the user interface and the server function for app:

shinyApp(ui = ui, server = server)
