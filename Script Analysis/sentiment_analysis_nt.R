library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

nt <- paste(readLines("./Script Analysis/NationalTreasureScript.htm"))

nt <- data.frame(nt, stringsAsFactors = FALSE) %>%
  filter(!nt %in% c("", " "))

tidy_nt <- nt %>%
  unnest_tokens(word, nt)

nt_sentiment <- tidy_nt %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

top_nt <- nt_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

top_nt %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

nt_sentiment2 <- tidy_nt %>%
  inner_join(get_sentiments("afinn")) %>%
  arrange(score) %>%
  mutate(average = mean(score))

nt_sentiment2 %>%
  ggplot(aes(x = score)) + geom_bar(fill = "skyblue")

nt_plot <- tidy_nt %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(row = row_number()) %>%
  count(index =  row %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

nt_plot %>%
  ggplot(aes(index, sentiment, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE)
