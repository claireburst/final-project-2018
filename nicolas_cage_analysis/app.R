library(shiny)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(plotly)

# Raising Arizona movie data
arizona_sentiment <- read_rds("./arizona_sentiment.rds")
arizona_sentiment2 <- read_rds("./arizona_sentiment2.rds")
arizona_plot <- read_rds("./arizona_plot.rds")

top_arizona <- arizona_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n)) %>%
  mutate(sentiment = fct_recode(sentiment, "Positive" = "positive", "Negative" = "negative"))

# The Croods movie data
croods_sentiment <- read_rds("./croods_sentiment.rds")
croods_sentiment2 <- read_rds("./croods_sentiment2.rds")
croods_plot <- read_rds("./croods_plot.rds")

top_croods <- croods_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n)) %>%
  mutate(sentiment = fct_recode(sentiment, "Positive" = "positive", "Negative" = "negative"))

# Face/Off movie data
faceoff_sentiment <- read_rds("./faceoff_sentiment.rds")
faceoff_sentiment2 <- read_rds("./faceoff_sentiment2.rds")
faceoff_plot <- read_rds("./faceoff_plot.rds")

top_faceoff <- faceoff_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n)) %>%
  mutate(sentiment = fct_recode(sentiment, "Positive" = "positive", "Negative" = "negative"))

# National Treasure movie data
nt_sentiment <- read_rds("./nt_sentiment.rds")
nt_sentiment2 <- read_rds("./nt_sentiment2.rds")
nt_plot <- read_rds("./nt_plot.rds")

top_nt <- nt_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n)) %>%
  mutate(sentiment = fct_recode(sentiment, "Positive" = "positive", "Negative" = "negative"))

# National Treasure 2 movie data
nt2_sentiment <- read_rds("./nt2_sentiment.rds")
nt2_sentiment2 <- read_rds("./nt2_sentiment2.rds")
nt2_plot <- read_rds("./nt2_plot.rds")

top_nt2 <- nt2_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n)) %>%
  mutate(sentiment = fct_recode(sentiment, "Positive" = "positive", "Negative" = "negative"))

movie_options <- c("Raising Arizona", 
               "The Croods",
               "Face/Off", 
               "National Treasure", 
               "National Treasure 2: Book of Secrets")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Nicolas Cage: an In-Depth Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "movie",
                    label = "Select a Nicolas Cage Movie",
                    choices = movie_options,
                    multiple = FALSE, 
                    selected = movie_options[1])
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Movie Analysis", plotOutput("topwords"), plotOutput("scorefreq"),
                             plotOutput("plot")),
                    tabPanel("Nicolas Cage vs. the World", plotOutput("bigpicture")),
                    tabPanel("About This App", htmlOutput("about")))
    
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$topwords <- renderPlot({
     if (input$movie == "Raising Arizona") {
       
     top_arizona %>%
         ggplot(aes(word, n, fill = sentiment)) +
         geom_col(show.legend = FALSE) +
         facet_wrap(~sentiment, scales = "free") +  
         coord_flip() +
         ggtitle(paste("The Most Common Positive and Negative Words in", input$movie),
                 subtitle = "After tallying all words in the script, these were the words of each sentiment expressed the most frequently.") +
         ylab("Number of Utterances") +
         xlab("Word")
     }
     else if (input$movie == "The Croods") {
       top_croods %>%
         ggplot(aes(word, n, fill = sentiment)) +
         geom_col(show.legend = FALSE) +
         facet_wrap(~sentiment, scales = "free") +  
         coord_flip() +
         ggtitle(paste("The Most Common Positive and Negative Words in", input$movie),
                 subtitle = "After tallying all words in the script, these were the words of each sentiment expressed the most frequently.") +
         ylab("Number of Utterances") +
         xlab("Word")
     }
     else if (input$movie == "Face/Off") {
       top_faceoff %>%
         ggplot(aes(word, n, fill = sentiment)) +
         geom_col(show.legend = FALSE) +
         facet_wrap(~sentiment, scales = "free") +  
         coord_flip() +
         ggtitle(paste("The Most Common Positive and Negative Words in", input$movie),
                 subtitle = "After tallying all words in the script, these were the words of each sentiment expressed the most frequently.") +
         ylab("Number of Utterances") +
         xlab("Word")
     }
     else if (input$movie == "National Treasure") {
       top_nt %>%
         ggplot(aes(word, n, fill = sentiment)) +
         geom_col(show.legend = FALSE) +
         facet_wrap(~sentiment, scales = "free") +  
         coord_flip() +
         ggtitle(paste("The Most Common Positive and Negative Words in", input$movie),subtitle = "After tallying all words in the script, these were the words of each sentiment expressed the most frequently.") +
         ylab("Number of Utterances") +
         xlab("Word")
     }
     else if (input$movie == "National Treasure 2: Book of Secrets") {
       top_nt2 %>%
         ggplot(aes(word, n, fill = sentiment)) +
         geom_col(show.legend = FALSE) +
         facet_wrap(~sentiment, scales = "free") +  
         coord_flip() +
         ggtitle(paste("The Most Common Positive and Negative Words in", input$movie),
                 subtitle = "After tallying all words in the script, these were the words of each sentiment expressed the most frequently.") +
         xlab("Number of Utterances") +
         ylab("Word")
     }
   })
   
   output$scorefreq <- renderPlot({
     
     if (input$movie == "Raising Arizona") {
       
       arizona_sentiment2 %>%
         ggplot(aes(x = score)) + 
         geom_bar(fill = "skyblue") +
         ggtitle(paste("The Frequency of Positive and Negative Words in", input$movie),
                 subtitle = "Words in the movie script were scored on a scale of -5 to 5, with -5 being MOST negative and 5 being MOST positive.") +
         xlab("Positivity/Negativity Score") +
         ylab("Number of Utterances")
     }
     else if (input$movie == "The Croods") {
       croods_sentiment2 %>%
         ggplot(aes(x = score)) + 
         geom_bar(fill = "skyblue") +
         ggtitle(paste("The Frequency of Positive and Negative Words in", input$movie),
                 subtitle = "Words in the movie script were scored on a scale of -5 to 5, with -5 being MOST negative and 5 being MOST positive.") +
         xlab("Positivity/Negativity Score") +
         ylab("Number of Utterances")
     }
     else if (input$movie == "Face/Off") {
       faceoff_sentiment2 %>%
         ggplot(aes(x = score)) + 
         geom_bar(fill = "skyblue") +
         ggtitle(paste("The Frequency of Positive and Negative Words in", input$movie),
                 subtitle = "Words in the movie script were scored on a scale of -5 to 5, with -5 being MOST negative and 5 being MOST positive.") +
         xlab("Positivity/Negativity Score") +
         ylab("Number of Utterances")
     }
     else if (input$movie == "National Treasure") {
       nt_sentiment2 %>%
         ggplot(aes(x = score)) + 
         geom_bar(fill = "skyblue") +
         ggtitle(paste("The Frequency of Positive and Negative Words in", input$movie),
                 subtitle = "Words in the movie script were scored on a scale of -5 to 5, with -5 being MOST negative and 5 being MOST positive.") +
         xlab("Positivity/Negativity Score") +
         ylab("Number of Utterances")
     }
     else if (input$movie == "National Treasure 2: Book of Secrets") {
       nt2_sentiment2 %>%
         ggplot(aes(x = score)) + 
         geom_bar(fill = "skyblue") +
         ggtitle(paste("The Frequency of Positive and Negative Words in", input$movie),
                 subtitle = "Words in the movie script were scored on a scale of -5 to 5, with -5 being MOST negative and 5 being MOST positive.") +
         xlab("Positivity/Negativity Score") +
         ylab("Number of Utterances")
     }
     
     
   })
   
   output$plot <- renderPlot({
     
     if (input$movie == "Raising Arizona") {
       arizona_plot %>%
         ggplot(aes(index, sentiment, fill = sentiment)) +
         geom_bar(stat = "identity", show.legend = FALSE) +
         ggtitle(paste("Variance in Emotion Throughout", input$movie, "Runtime"), 
                 subtitle = "This plot illustrates the density of emotion words over time in the film. The darker the colors, the more extreme the sentiments.") +
         xlab("Runtime") +
         ylab("Positivity/Negativity of Expression") +
         theme(axis.text.x=element_blank())
       
     }
     else if (input$movie == "The Croods") {
       croods_plot %>%
         ggplot(aes(index, sentiment, fill = sentiment)) +
         geom_bar(stat = "identity", show.legend = FALSE) +
         ggtitle(paste("Variance in Emotion Throughout", input$movie, "Runtime"), 
                 subtitle = "This plot illustrates the density of emotion words over time in the film. The darker the colors, the more extreme the sentiments.") +
         xlab("Runtime") +
         ylab("Positivity/Negativity of Expression") +
         theme(axis.text.x=element_blank())
     }
     else if (input$movie == "Face/Off") {
       faceoff_plot %>%
         ggplot(aes(index, sentiment, fill = sentiment)) +
         geom_bar(stat = "identity", show.legend = FALSE) +
         ggtitle(paste("Variance in Emotion Throughout", input$movie, "Runtime"), 
                 subtitle = "This plot illustrates the density of emotion words over time in the film. The darker the colors, the more extreme the sentiments.") +
         xlab("Runtime") +
         ylab("Positivity/Negativity of Expression") +
         theme(axis.text.x=element_blank())
     }
     else if (input$movie == "National Treasure") {
       nt_plot %>%
         ggplot(aes(index, sentiment, fill = sentiment)) +
         geom_bar(stat = "identity", show.legend = FALSE) +
         ggtitle(paste("Variance in Emotion Throughout", input$movie, "Runtime"), 
                 subtitle = "This plot illustrates the density of emotion words over time in the film. The darker the colors, the more extreme the sentiments.") +
         xlab("Runtime") +
         ylab("Positivity/Negativity of Expression") +
         theme(axis.text.x=element_blank())
     }
     else if (input$movie == "National Treasure 2: Book of Secrets") {
       nt2_plot %>%
         ggplot(aes(index, sentiment, fill = sentiment)) +
         geom_bar(stat = "identity", show.legend = FALSE) +
         ggtitle(paste("Variance in Emotion Throughout", input$movie, "Runtime"), 
                 subtitle = "This plot illustrates the density of emotion words over time in the film. The darker the colors, the more extreme the sentiments.") +
         xlab("Runtime") +
         ylab("Positivity/Negativity of Expression") +
         theme(axis.text.x=element_blank())
     }
   })
   
   output$about <- renderUI({
     "Hello testing testing!"
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

