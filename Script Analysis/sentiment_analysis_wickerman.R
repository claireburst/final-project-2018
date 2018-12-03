library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

# Read in movie script from file 

wicker <- paste(readLines("./Script Analysis/WickermanScript.html"))

# Creates dataframe where each line of script is a row in the data frame
# We don't want these rows to be factors, so set stringsAsFactors = FALSE
# Finally, filter to avoid weird formatting things that may appear in data

wicker <- data.frame(wicker, stringsAsFactors = FALSE) %>%
  filter(!wicker %in% c("", "</br>"))

# Tidying the data such that each row a singular word from the script, to allow for 
# sentiment analysis

tidy_wicker <- wicker %>%
  unnest_tokens(word, wicker) %>%
  filter(word != "br")

# Okay, all of these sentiment_analysis_movie.R files are the same, so I'm going to use the
# same comments in all of them because I do  the exact same things in each. 
# I create various tibbles joining the words from the script with 
# different sentiment directories e.g. bing and afinn. Then, I use various plots to attempt to 
# Visualize the frequencies of words and sentiments. I also write several rds files so that I can
# Read them in to my app.R files. 

wicker_sentiment <- tidy_wicker %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

write_rds(wicker_sentiment, "nicolas_cage_analysis/RDS Files/wicker_sentiment.rds")

top_wicker <- wicker_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

top_wicker %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

wicker_sentiment2 <- tidy_wicker %>%
  inner_join(get_sentiments("afinn")) %>%
  arrange(score) %>%
  mutate(average = mean(score))

write_rds(wicker_sentiment2, "nicolas_cage_analysis/RDS Files/wicker_sentiment2.rds")

wicker_sentiment2 %>%
  ggplot(aes(x = score)) + geom_bar(fill = "skyblue")

wicker_plot <- tidy_wicker %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(row = row_number()) %>%
  count(index =  row %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

write_rds(wicker_plot, "nicolas_cage_analysis/RDS Files/wicker_plot.rds")

wicker_plot %>%
  ggplot(aes(index, sentiment, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE)

