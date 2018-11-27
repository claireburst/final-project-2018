library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

wicker <- paste(readLines("./Script Analysis/WickermanScript.html"))

wicker <- data.frame(wicker, stringsAsFactors = FALSE) %>%
  filter(!wicker %in% c("", "</br>"))

tidy_wicker <- wicker %>%
  unnest_tokens(word, wicker) %>%
  filter(word != "br")

wicker_sentiment <- tidy_wicker %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

write_rds(wicker_sentiment, "nicolas_cage_analysis/wicker_sentiment.rds")

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

write_rds(wicker_sentiment2, "nicolas_cage_analysis/wicker_sentiment2.rds")

wicker_sentiment2 %>%
  ggplot(aes(x = score)) + geom_bar(fill = "skyblue")

wicker_plot <- tidy_wicker %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(row = row_number()) %>%
  count(index =  row %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

write_rds(wicker_plot, "nicolas_cage_analysis/wicker_plot.rds")

wicker_plot %>%
  ggplot(aes(index, sentiment, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE)

