library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

croods <- paste(readLines("CroodsMovieScript.html"))

croods <- data.frame(croods, stringsAsFactors = FALSE) %>%
  filter(!croods %in% c("", "</br>"))

tidy_croods <- croods %>%
  unnest_tokens(word, croods) %>%
  filter(word != "br")

croods_sentiment <- tidy_croods %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

top_croods <- croods_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

top_croods %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

croods_sentiment2 <- tidy_croods %>%
  inner_join(get_sentiments("afinn")) %>%
  arrange(score) %>%
  mutate(average = mean(score))

croods_sentiment2 %>%
  ggplot(aes(x = score)) + geom_bar(fill = "skyblue")


croods_plot <- tidy_croods %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(row = row_number()) %>%
  count(index =  row %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

croods_plot %>%
  ggplot(aes(index, sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE)

