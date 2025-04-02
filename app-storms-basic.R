# Stat 133
# Author: Gaston Sanchez
# Description: Shiny app that plots storms in the North Atlantic 
#              for a given year
# Data: "storms" (from dplyr)

# required packages
library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)

# world map data for ggplot()
world_countries = ne_countries(returnclass = "sf")

# map to be used as our canvas
atlantic_map = ggplot(data = world_countries) +
  geom_sf(fill = "gray95") +
  coord_sf(xlim = c(-110, 0), ylim = c(5, 65)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(x = "", y = "")


# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Storms in North Atlantic"),
  
  # Sidebar with input widgets
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                  label = "Year",
                  sep = "", # no separator between thousands places
                  min = 1975,
                  max = 2021,
                  value = 2000),
      checkboxInput(inputId = "hurricanes",
                    label = "Only hurricanes",
                    value = FALSE)
    ),
    
    # Panel with map, and summary table
    mainPanel(
      plotOutput(outputId = "plot_map", height = "500px"),
      hr(),
      tableOutput(outputId = "summary_table")
    )
  )
)


# Define server
server <- function(input, output) {
  
  # reactive table of filtered storms
  tbl = reactive({
    dat = storms %>% 
      filter(year == input$year)
    
    if (input$hurricanes) {
      dat = dat %>% filter(wind >= 64)
    }
    
    dat
  })
  
  
  # map of storms
  output$plot_map <- renderPlot({
    
    # adding storm locations (lat & long)
    p = atlantic_map +
      geom_point(data = tbl(), 
                 aes(x=long, y=lat, color=name, alpha = 0.7)) +
      geom_path(data = tbl(), aes(x=long, y=lat, color=name)) +
      guides(alpha = "none") +
      labs(title = paste("Storms in", input$year))

    # map output
    p
  })
  
  
  # summary table (maximum wind speed)
  output$summary_table <- renderTable({
    tbl() %>%
      group_by(name) %>%
      summarise(max_wind = max(wind))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
