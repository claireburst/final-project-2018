library(shiny)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggrepel)
library(plotly)
library(scales)
library(janitor)

# Adaptation movie data
adaptation_sentiment <- read_rds("./adaptation_sentiment.rds")
adaptation_sentiment2 <- read_rds("./adaptation_sentiment2.rds")
adaptation_plot <- read_rds("./adaptation_plot.rds")

top_adaptation <- adaptation_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n)) %>%
  mutate(sentiment = fct_recode(sentiment, "Positive" = "positive", "Negative" = "negative"))

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

# Con Air movie data
con_sentiment <- read_rds("./con_sentiment.rds")
con_sentiment2 <- read_rds("./con_sentiment2.rds")
con_plot <- read_rds("./con_plot.rds")

top_con <- con_sentiment %>%
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

# Leaving Las Vegas movie data
llv_sentiment <- read_rds("./llv_sentiment.rds")
llv_sentiment2 <- read_rds("./llv_sentiment2.rds")
llv_plot <- read_rds("./llv_plot.rds")

top_llv <- llv_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n)) %>%
  mutate(sentiment = fct_recode(sentiment, "Positive" = "positive", "Negative" = "negative"))

# Moonstruck movie data
moon_sentiment <- read_rds("./moon_sentiment.rds")
moon_sentiment2 <- read_rds("./moon_sentiment2.rds")
moon_plot <- read_rds("./moon_plot.rds")

top_moon <- moon_sentiment %>%
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

# The Wicker Man movie data
wicker_sentiment <- read_rds("./wicker_sentiment.rds")
wicker_sentiment2 <- read_rds("./wicker_sentiment2.rds")
wicker_plot <- read_rds("./wicker_plot.rds")

top_wicker <- wicker_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n)) %>%
  mutate(sentiment = fct_recode(sentiment, "Positive" = "positive", "Negative" = "negative"))

# Vector for the drop down allowing user to select a movie
movie_options <- c("Adaptation",
               "Con Air",
               "The Croods",
               "Face/Off",
               "Leaving Las Vegas",
               "Moonstruck",
               "National Treasure", 
               "National Treasure 2: Book of Secrets",
               "Raising Arizona",
               "The Wicker Man")

# Defining tibbles for the big movie analysis tab
allmovies <- read_excel("./NIC_CAGE.xlsx")

allmovies <- allmovies %>%
  clean_names() %>%
  select(movie, total_box_office, theatrical_release_release_date, running_time, mpaa, metacritic, sentiment)

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
                    tabPanel("Movie Textual Analysis", plotOutput("topwords"), plotOutput("scorefreq"),
                             plotOutput("plot")),
                    tabPanel("All Nicolas Cage Movies", plotOutput("mpaacount"), plotlyOutput("time"), 
                             htmlOutput("explain"),
                             plotlyOutput("metacritic")),
                    tabPanel("About This App", htmlOutput("about")))
    
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$time <- renderPlotly({
     
     font <- list(
       family = "helvetica",
       size = 12,
       color = 'black')
     
     ggplotly(tooltip = c("text"),
       ggplot(data = allmovies, aes(x = theatrical_release_release_date, 
          y = total_box_office, color = mpaa, text = movie)) + 
          geom_point() +
          scale_y_continuous(labels = comma) +
          labs(color = "MPAA Rating") + 
          ylab("Total Box Office Revenue") +
          xlab("Theatrical Release Date") +
          ggtitle("Total Box Office Revenue Over Time",
                  subtitle = "While revenue generally improved over time, a further analysis shows PG rated movies generated much more revenue over time while PG-13 and R-rated revenue correlations do not appear to be significant.")) %>%
       layout(title = "Total Box Office Revenue Over Time",
              font = font)
     
   })
   
   # Had to add this because you're unable to add subtitles in plotly :( So I needed text blurbs
   output$explain <- renderUI({
     
     explain <- paste("\n While revenue generally improved over time, a further analysis shows PG rated movies generated much more revenue over time while PG-13 and R-rated revenue correlations do not appear to be significant.")
     
     HTML(paste(tags$ul(p(explain))))
   })
   
   output$metacritic <- renderPlotly({
     
     font <- list(
       family = "helvetica",
       size = 12,
       color = 'black')
     
     ggplotly(tooltip = c("text"),
              ggplot(data = allmovies, aes(x = theatrical_release_release_date, 
                                           y = metacritic, color = mpaa, text = movie)) + 
                geom_point() +  
                scale_y_continuous(labels = comma) +
                labs(color = "MPAA Rating") + 
                ylab("Metacritic Score") +
                xlab("Theatrical Release Date")) %>%
       layout(title = "Metacritic Scores Over Time",
              font = font)
     
   })
   
   output$mpaacount <- renderPlot({
     
     allmovies %>%
       ggplot(aes(x = mpaa, fill = mpaa)) +
       geom_bar() +
       xlab("MPAA Rating") +
       ylab("Number of Films") +
       labs(color = "MPAA Rating") + 
       ggtitle("Number of Movies by MPAA Rating",
               subtitle = "Nicolas Cage has done more R rated movies than PG and PG-13 combined")
     
   })
   
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
     else if (input$movie == "Adaptation") {
       top_adaptation %>%
         ggplot(aes(word, n, fill = sentiment)) +
         geom_col(show.legend = FALSE) +
         facet_wrap(~sentiment, scales = "free") +  
         coord_flip() +
         ggtitle(paste("The Most Common Positive and Negative Words in", input$movie),
                 subtitle = "After tallying all words in the script, these were the words of each sentiment expressed the most frequently.") +
         ylab("Number of Utterances") +
         xlab("Word")
     }
     else if (input$movie == "Con Air") {
       top_con %>%
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
     else if (input$movie == "Leaving Las Vegas") {
       top_llv %>%
         ggplot(aes(word, n, fill = sentiment)) +
         geom_col(show.legend = FALSE) +
         facet_wrap(~sentiment, scales = "free") +  
         coord_flip() +
         ggtitle(paste("The Most Common Positive and Negative Words in", input$movie),subtitle = "After tallying all words in the script, these were the words of each sentiment expressed the most frequently.") +
         ylab("Number of Utterances") +
         xlab("Word")
     }
     else if (input$movie == "Moonstruck") {
       top_moon %>%
         ggplot(aes(word, n, fill = sentiment)) +
         geom_col(show.legend = FALSE) +
         facet_wrap(~sentiment, scales = "free") +  
         coord_flip() +
         ggtitle(paste("The Most Common Positive and Negative Words in", input$movie),subtitle = "After tallying all words in the script, these were the words of each sentiment expressed the most frequently.") +
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
     else if (input$movie == "The Wicker Man") {
       top_wicker %>%
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
     else if (input$movie == "Adaptation") {
       adaptation_sentiment2 %>%
         ggplot(aes(x = score)) + 
         geom_bar(fill = "skyblue") +
         ggtitle(paste("The Frequency of Positive and Negative Words in", input$movie),
                 subtitle = "Words in the movie script were scored on a scale of -5 to 5, with -5 being MOST negative and 5 being MOST positive.") +
         xlab("Positivity/Negativity Score") +
         ylab("Number of Utterances")
     }
     else if (input$movie == "Con Air") {
       con_sentiment2 %>%
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
     else if (input$movie == "Leaving Las Vegas") {
       llv_sentiment2 %>%
         ggplot(aes(x = score)) + 
         geom_bar(fill = "skyblue") +
         ggtitle(paste("The Frequency of Positive and Negative Words in", input$movie),
                 subtitle = "Words in the movie script were scored on a scale of -5 to 5, with -5 being MOST negative and 5 being MOST positive.") +
         xlab("Positivity/Negativity Score") +
         ylab("Number of Utterances")
     }
     else if (input$movie == "Moonstruck") {
       moon_sentiment2 %>%
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
     else if (input$movie == "The Wicker Man") {
       wicker_sentiment2 %>%
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
     else if (input$movie == "Adaptation") {
       adaptation_plot %>%
         ggplot(aes(index, sentiment, fill = sentiment)) +
         geom_bar(stat = "identity", show.legend = FALSE) +
         ggtitle(paste("Variance in Emotion Throughout", input$movie, "Runtime"), 
                 subtitle = "This plot illustrates the density of emotion words over time in the film. The darker the colors, the more extreme the sentiments.") +
         xlab("Runtime") +
         ylab("Positivity/Negativity of Expression") +
         theme(axis.text.x=element_blank())
     }
     else if (input$movie == "Con Air") {
       con_plot %>%
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
     else if (input$movie == "Leaving Las Vegas") {
       llv_plot %>%
         ggplot(aes(index, sentiment, fill = sentiment)) +
         geom_bar(stat = "identity", show.legend = FALSE) +
         ggtitle(paste("Variance in Emotion Throughout", input$movie, "Runtime"), 
                 subtitle = "This plot illustrates the density of emotion words over time in the film. The darker the colors, the more extreme the sentiments.") +
         xlab("Runtime") +
         ylab("Positivity/Negativity of Expression") +
         theme(axis.text.x=element_blank())
     }
     else if (input$movie == "Moonstruck") {
       moon_plot %>%
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
     else if (input$movie == "The Wicker Man") {
       wicker_plot %>%
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
     str1 <- paste("About This App")
     str2 <- paste("This app was created by Claire Fridkin as the final project for GOV1005: Data at Harvard University in Fall 2018.")
     str3 <- paste("The goal of this app was to analyze Nicolas Cage's movie career through movie scripts and the success and revenue of his films.")
     str4 <- paste("Special thanks to Walter Hickey of FiveThirtyEight for providing me with baseline data. I used IMDB to find movie runtimes and Metacritic scores.")
     str5 <- paste("A cautionary warning for interpreting sentiment analysis:")
     str6 <- paste("some words may have duplicate meanings (e.g. 'like' used as a verbal crutch vs. as a verb). This may have influenced the data and should be considered in your interpretations!")
     str7 <- paste("Also note: movies were excluded from analysis ofNicolas Cage only played a supporting roles. This left 54 movies to be examined (I did not consider movies beyond 2013). In the Metacritic score plot, only 46 movies appear because 8 movies did not have Metacritic scores on IMDB.")
     
     HTML(paste(tags$ul(h3(str1)), p(str2), p(str3), p(str4), strong(em(str5)), str6, p(p(str7))))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

