#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyverse)
library(readxl)
library(janitor)
library(knitr)

data <- read_rds("data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Nicolas Cage Box Office Revenue"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
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
      # generate bins based on input$bins from ui.R
      x <- data$total_box_office 
      y <- data$running_time
      
      # draw the histogram with the specified number of bins
      ggplot(data, aes(x, y)) + 
        geom_point() + 
        xlab("Movie Box Office Revenue") + 
        ylab("Movie Runtime (Minutes)") 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

