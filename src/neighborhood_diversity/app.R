#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(readr)
library(dplyr)

data_by_dist <- read_rds("data/diverse_data_by_dist.rds")
data_by_year <- read_csv("data/diverse_data_by_year.csv")
metro_names <- data_by_dist |> pull(metro_name) |> unique()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Neighborhood Diversity"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

        sidebarPanel(

          selectInput(
              "city",
              "City Name",
              metro_names,
              selected = metro_names[1],
              multiple = FALSE
            ),

            sliderInput(
                        inputId = "span",
                        label = "Span parameter:",
                        min = 0.1,
                        max = 1,
                        value = 0.5,
                        step = 0.05)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        city_data <- data_by_year |> filter(metro_name == input$city)

        fig <- ggplot(city_data, aes(x = distmiles, y = entropy)) +
          geom_point(alpha = 0.4) +
          geom_smooth(method = "loess", span = input$span, se = FALSE, color = "red") +
          labs(
            title = paste("Diversity Gradient for", input$city, "(2020 US Census)"),
            x = "Distance From City Hall (Miles)",
            y = "Diversity Score"
          )

        ggplotly(fig)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
