library(tidyverse)
library(shiny)
library(DT)
library(bslib) #fonts


#do data cleaning and r stuff above here

my_theme <- bs_theme(bootswatch = 'vapor') %>%
  bs_theme_update(bg = 'rgb(235,175,175)', fg = 'rgb(63,11,11)',
                  primary = '#B5C142', secondary = '#575155', info ='#103851',
                  base_font = font_google('Zilla Slab'),
                  code_font = font_google('Syne Mono'),
                  heading_font = font_google('Montsterrat Alternatives'), 
                  font_scale = 1.3)

### create the userinterface

ui <- fluidPage(
  theme = bs_theme(bootswatch = 'minty'), 
  
  titlePanel("Santa Barbara County Birding App"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Widgets"),
      
      textInput("text", label = h4("Search Bird Species"), value = "Enter text..."),
      
      hr(),
      
      selectInput("select", label = h4("Select Species"), 
                  choices = c("California Scrub-Jay", "Anna’s Hummingbird", "Western Bluebird"), 
                  selected = "California Scrub-Jay"),
      
      hr(),
      
      sliderInput(
        inputId = "year",
        label = h4("Select Year:"),
        min = 1990,
        max = 2024,
        value = 2024,
        step = 1,
        animate = TRUE
      ), 
      
      hr(),
      
      checkboxGroupInput(
        inputId = "bird_type", 
        label = h4("Choose Bird Type:"), 
        choices = c("Resident", "Winter Migrant", "Summer Migrant"), 
        selected = c("Resident", "Winter Migrant", "Summer Migrant")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        # Overview Tab
        tabPanel("Overview", 
                 h2("About This App"),
                 p("This interactive app allows users to explore bird populations in Santa Barbara County 
             over time. Users can search for specific bird species, filter by migration type, and 
             visualize population trends."),
                 
                 h3("Dataset Summary"),
                 p("The dataset includes bird observation records collected from various citizen science 
             initiatives, research stations, and local surveys. The data spans from 1990 to 2024 
             and includes both resident and migratory bird species."),
                 
                 h3("How to Use"),
                 p("1. Use the search box to find a specific bird species."),
                 p("2. Select a bird from the dropdown menu."),
                 p("3. Use the slider to choose a year range."),
                 p("4. Check the boxes to filter by resident or migratory birds."),
                 p("5. Navigate to the 'Visualization' tab to see the graph, or the 'Data Table' tab 
             to explore the dataset.")
        ),
        
        # Visualization Tab
        tabPanel("Visualization",
                 h2("Bird Observations Over Time"),
                 plotOutput(outputId = "penguin_plot")
        ),
        
        # Data Table Tab
        tabPanel("Data Table",
                 h2("Bird Observation Dataset"),
                 DTOutput("bird_data_table")
        )
      )
    )
  )
)

### create the server function

server <- function(input, output) {
  
  
  output$diceroll <- reactive({
    x <- input$dice_button
    
    rolls <- sample(1:6, size = 2, replace = TRUE)
    
    txt_out <- sprintf('Die 1: %s, die 2: %s, total = %s', rolls[1], rolls[2], sum(rolls))
  })
  
  
  
  
  penguin_select <- reactive({
    penguins %>% 
      filter(species == input$penguins_species)
  })
  
  output$penguin_plot <- renderPlot({
    ggplot(data = penguin_select()) +
      geom_point( aes(x = flipper_length_mm, y = body_mass_g),
                  color = input$pt_color)
  })
  
  penguin_sum_table <- reactive({
    penguin_summary_df <- penguins |>
      filter(species == input$penguins_species) |>
      group_by(sex) |>
      summarize(mean_flop = mean(flipper_length_mm, na.rm = TRUE),
                mean_mass = mean(body_mass_g, na.rm = TRUE))
  })
  
  output$penguin_table <- renderTable({
    penguin_sum_table()
  })
}

### combine the userinterface and the server function for app:

shinyApp(ui = ui, server = server)