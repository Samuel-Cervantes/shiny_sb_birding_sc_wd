library(tidyverse)
library(shiny)
library(DT)
library(leaflet)
library(bslib) #fonts
library(here)
library(sf)

#do data cleaning and r stuff above here
bird_data_top <- readRDS(here("data", "bird_data_top.rds"))

# Function to determine season based on month
determine_season <- function(date) {
  month <- lubridate::month(date)
  case_when(
    month %in% c(12, 1, 2) ~ "Winter",
    month %in% c(3, 4, 5) ~ "Spring",
    month %in% c(6, 7, 8) ~ "Summer",
    month %in% c(9, 10, 11) ~ "Fall"
  )
}

# Add season column to dataset
bird_data_top <- bird_data_top %>% 
  mutate(season = determine_season(observation_date))

# Count of each bird species observed in the dataset
species_counts <- bird_data_top |> 
  count(common_name, observation_date, name = "Total_Observations")

# Monthly observations for facet wrap
species_monthly_counts <- bird_data_top |> 
  mutate(month = lubridate::month(observation_date, label = TRUE),
         year = lubridate::year(observation_date)) |> 
  count(common_name, month, year, name = "Monthly_Observations")

# Get unique species names for the select input
species_list <- unique(bird_data_top$common_name)

santa_barbara_geo <- read_sf(here("data", "ca_counties", "CA_Counties_TIGER2016.shp")) |> 
  filter(NAME == "Santa Barbara") |> 
  st_transform(4326)

### create the user interface

ui <- navbarPage(
  theme = bs_theme(
    bootswatch = 'flatly',  # A professional theme with blue, green, and orange tones
    primary = "#2c7fb8",   # Blue for primary elements
    secondary = "#74c476", # Green for accents
    success = "#ff964f",   # Orange as an additional highlight color
    font_scale = 1.1
  ),
  "Santa Barbara County Birding App",
  
  tabPanel("Overview",
           h2("How to Use This App"),
           p("1. Navigate to the 'Map' tab to explore geographic bird observations. Use the dropdown menu to select a species, adjust the year slider, and filter by season to refine your view."),
           p("2. In the 'Visualization' tab, analyze species trends over time. The first graph shows total observations over the years, while the second graph provides a breakdown of monthly observations within a selected year."),
           p("3. The 'Data Table' tab provides access to the full dataset of bird observations. You can explore raw data, search for specific species, or export data for further analysis."),
           p("Whether you are a researcher, conservationist, or bird enthusiast, this app is designed to help you better understand avian biodiversity in Santa Barbara County.")),
  
  tabPanel("Map",
           h2("Map of Mainland Santa Barbara County"),
           p("Explore bird observations in mainland Santa Barbara County."),
           selectInput("select", label = h4("Select Species"), choices = species_list, selected = species_list[1]),
           sliderInput("year", label = h4("Select Year:"), min = 2000, max = 2024, value = 2024, step = 1, animate = TRUE, sep = ""),
           checkboxGroupInput("season", label = h4("Select Seasons"), 
                              choices = c("Winter", "Spring", "Summer", "Fall"), 
                              selected = c("Winter", "Spring", "Summer", "Fall")),
           tableOutput("observations_summary"),
           leafletOutput("sb_map", height = "600px")),
  
  tabPanel("Visualization",
           h2("Species Count Over Time"),
           selectInput("select_vis", label = h4("Select Species"), choices = species_list, selected = species_list[1]),
           plotOutput(outputId = "species_count_plot"),
           h2("Monthly Observations by Species"),
           sliderInput("vis_year", label = h4("Select Year for Monthly Observations:"), min = 2000, max = 2024, value = 2024, step = 1, animate = TRUE, sep = ""),
           plotOutput(outputId = "species_monthly_plot")),
  
  tabPanel("Data Table",
           h2("Bird Observation Dataset"),
           DTOutput("bird_data_table"))
)

### create the server function

server <- function(input, output) {
  filtered_data <- reactive({
    bird_data_top %>% 
      filter(common_name == input$select, 
             year(observation_date) == input$year,
             season %in% input$season)
  })
  
  output$observations_summary <- renderTable({
    filtered_data() %>% 
      summarise(Total_Observations = n())
  })
  
  output$sb_map <- renderLeaflet({
    leaflet(data = santa_barbara_geo) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolygons(
        fillColor = NA,
        color = "black",
        weight = 2,
        opacity = 1,
        fillOpacity = 0,
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
        popup = ~paste("Species:", common_name, "<br>Year:", year(observation_date), "<br>Season:", season)
      ) %>%
      setView(lng = -119.7, lat = 34.5, zoom = 9)
  })
  
  output$species_count_plot <- renderPlot({
    species_counts %>%
      filter(common_name == input$select_vis) %>%
      ggplot(aes(x = observation_date, y = Total_Observations)) +
      geom_line() +
      labs(title = "Total Observations Over Time",
           x = "Date",
           y = "Total Observations") +
      theme_minimal()
  })
  
  output$species_monthly_plot <- renderPlot({
    species_monthly_counts %>%
      filter(common_name == input$select_vis, year == input$vis_year) %>%
      ggplot(aes(x = month, y = Monthly_Observations, fill = month)) +
      geom_col() +
      labs(title = "Monthly Observations",
           x = "Month",
           y = "Total Observations") +
      theme_minimal()
  })
  
  output$bird_data_table <- renderDT({
    datatable(bird_data_top)
  })
}

### combine the user interface and the server function for app:

shinyApp(ui = ui, server = server)