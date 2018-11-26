library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

faceoff <- paste(readLines("./Script Analysis/FaceOffScript.htm"))

faceoff <- data.frame(faceoff, stringsAsFactors = FALSE) %>%
  filter(faceoff != "")
  
tidy_faceoff <- faceoff %>%
  unnest_tokens(word, faceoff)

faceoff_sentiment <- tidy_faceoff %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

write_rds(faceoff_sentiment, "nicolas_cage_analysis/faceoff_sentiment.rds")

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

write_rds(faceoff_sentiment2, "nicolas_cage_analysis/faceoff_sentiment2.rds")

faceoff_sentiment2 %>%
  ggplot(aes(x = score)) + geom_bar(fill = "skyblue")

faceoff_plot <- tidy_faceoff %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(row = row_number()) %>%
  count(index =  row %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

write_rds(faceoff_plot, "nicolas_cage_analysis/faceoff_plot.rds")

faceoff_plot %>%
  ggplot(aes(index, sentiment, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE)
