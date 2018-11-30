library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

# Read in movie script from file 

con <- paste(readLines("./Script Analysis/ConAirScript.html"))

# Creates dataframe where each line of script is a row in the data frame
# We don't want these rows to be factors, so set stringsAsFactors = FALSE
# Finally, filter to avoid weird formatting things that may appear in data 

con <- data.frame(con, stringsAsFactors = FALSE) %>%
  filter(!con %in% c("", "</br>", "<p>"))

# Tidying the data such that each row a singular word from the script, to allow for 
# sentiment analysis

tidy_con <- con %>%
  unnest_tokens(word, con) %>%
  filter(word != "br")

# Okay, all of these sentiment_analysis_movie.R files are the same, so I'm going to use the
# same comments in all of them because I do  the exact same things in each. 
# I create various tibbles joining the words from the script with 
# different sentiment directories e.g. bing and afinn. Then, I use various plots to attempt to 
# Visualize the frequencies of words and sentiments. I also write several rds files so that I can
# Read them in to my app.R files. 

con_sentiment <- tidy_con %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

write_rds(con_sentiment, "nicolas_cage_analysis/con_sentiment.rds")

top_con <- con_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

top_con %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

con_sentiment2 <- tidy_con %>%
  inner_join(get_sentiments("afinn")) %>%
  arrange(score) %>%
  mutate(average = mean(score))

write_rds(con_sentiment2, "nicolas_cage_analysis/con_sentiment2.rds")

con_sentiment2 %>%
  ggplot(aes(x = score)) + geom_bar(fill = "skyblue")

con_plot <- tidy_con %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(row = row_number()) %>%
  count(index =  row %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

write_rds(con_plot, "nicolas_cage_analysis/con_plot.rds")

con_plot %>%
  ggplot(aes(index, sentiment, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE)
