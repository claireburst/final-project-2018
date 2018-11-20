library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

nt2 <- paste(readLines("./Script Analysis/NationalTreasure2Script.htm"))

nt2 <- data.frame(nt2, stringsAsFactors = FALSE) %>%
  filter(nt2 != "")

tidy_nt2 <- nt2 %>%
  unnest_tokens(word, nt2)

nt2_sentiment <- tidy_nt2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

top_nt2 <- nt2_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

top_nt2 %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

nt2_sentiment2 <- tidy_nt2 %>%
  inner_join(get_sentiments("afinn")) %>%
  arrange(score) %>%
  mutate(average = mean(score))

nt2_sentiment2 %>%
  ggplot(aes(x = score)) + geom_bar(fill = "skyblue")


nt2_plot <- tidy_nt2 %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(row = row_number()) %>%
  count(index =  row %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

nt2_plot %>%
  ggplot(aes(index, sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE)

