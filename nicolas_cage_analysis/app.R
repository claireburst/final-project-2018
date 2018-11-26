library(shiny)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(plotly)

# Raising Arizona movie data
arizona_sentiment <- read_rds("./nicolas_cage_analysis/arizona_sentiment.rds")
arizona_sentiment2 <- read_rds("./nicolas_cage_analysis/arizona_sentiment2.rds")
arizona_plot <- read_rds("./nicolas_cage_analysis/arizona_plot.rds")

# The Croods movie data
croods_sentiment <- read_rds("./nicolas_cage_analysis/croods_sentiment.rds")
croods_sentiment2 <- read_rds("./nicolas_cage_analysis/croods_sentiment2.rds")
croods_plot <- read_rds("./nicolas_cage_analysis/croods_plot.rds")

# Face/Off movie data
faceoff_sentiment <- read_rds("./nicolas_cage_analysis/faceoff_sentiment.rds")
faceoff_sentiment2 <- read_rds("./nicolas_cage_analysis/faceoff_sentiment2.rds")
faceoff_plot <- read_rds("./nicolas_cage_analysis/faceoff_plot.rds")

# National Treasure movie data
nt_sentiment <- read_rds("./nicolas_cage_analysis/nt_sentiment.rds")
nt_sentiment2 <- read_rds("./nicolas_cage_analysis/nt_sentiment2.rds")
nt_plot <- read_rds("./nicolas_cage_analysis/nt_plot.rds")

# National Treasure 2 movie data
nt2_sentiment <- read_rds("./nicolas_cage_analysis/nt2_sentiment.rds")
nt2_sentiment2 <- read_rds("./nicolas_cage_analysis/nt2_sentiment2.rds")
nt2_plot <- read_rds("./nicolas_cage_analysis/nt2_plot.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
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
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

