library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

adaptation <- paste(readLines("./Script Analysis/AdaptationScript.html"))

adaptation <- data.frame(adaptation, stringsAsFactors = FALSE) %>%
  filter(!adaptation %in% c("", "</br>", "<p>"))

tidy_adaptation <- adaptation %>%
  unnest_tokens(word, adaptation) %>%
  filter(word != "br")

adaptation_sentiment <- tidy_adaptation %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

write_rds(adaptation_sentiment, "nicolas_cage_analysis/adaptation_sentiment.rds")

top_adaptation <- adaptation_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

top_adaptation %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

adaptation_sentiment2 <- tidy_adaptation %>%
  inner_join(get_sentiments("afinn")) %>%
  arrange(score) %>%
  mutate(average = mean(score))

write_rds(adaptation_sentiment2, "nicolas_cage_analysis/adaptation_sentiment2.rds")

adaptation_sentiment2 %>%
  ggplot(aes(x = score)) + geom_bar(fill = "skyblue")

adaptation_plot <- tidy_adaptation %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(row = row_number()) %>%
  count(index =  row %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

write_rds(adaptation_plot, "nicolas_cage_analysis/adaptation_plot.rds")

adaptation_plot %>%
  ggplot(aes(index, sentiment, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE)
