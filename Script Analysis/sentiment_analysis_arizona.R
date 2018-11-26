library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

arizona <- paste(readLines("./Script Analysis/RaisingArizonaScript.html"))

arizona <- data.frame(arizona, stringsAsFactors = FALSE) %>%
  filter(!arizona %in% c("", "</br>"))

tidy_arizona <- arizona %>%
  unnest_tokens(word, arizona) %>%
  filter(word != "br")

arizona_sentiment <- tidy_arizona %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

write_rds(arizona_sentiment, "nicolas_cage_analysis/arizona_sentiment.rds")

top_arizona <- arizona_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

top_arizona %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

arizona_sentiment2 <- tidy_arizona %>%
  inner_join(get_sentiments("afinn")) %>%
  arrange(score) %>%
  mutate(average = mean(score))

write_rds(arizona_sentiment2, "nicolas_cage_analysis/arizona_sentiment2.rds")

arizona_sentiment2 %>%
  ggplot(aes(x = score)) + geom_bar(fill = "skyblue")


arizona_plot <- tidy_arizona %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(row = row_number()) %>%
  count(index =  row %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

write_rds(arizona_plot, "nicolas_cage_analysis/arizona_plot.rds")

arizona_plot %>%
  ggplot(aes(index, sentiment, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE)

