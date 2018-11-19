library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

faceoff <- paste(readLines("FaceOffScript.htm"))

faceoff <- data.frame(faceoff, stringsAsFactors = FALSE) %>%
  filter(faceoff != "")
  
tidy_faceoff <- faceoff %>%
  unnest_tokens(word, faceoff)

faceoff_sentiment <- tidy_faceoff %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

top_faceoff <- faceoff_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

top_faceoff %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

faceoff_sentiment2 <- tidy_faceoff %>%
  inner_join(get_sentiments("afinn")) %>%
  arrange(score) %>%
  mutate(average = mean(score))

faceoff_sentiment2 %>%
  ggplot(aes(x = score)) + geom_bar(fill = "skyblue")

