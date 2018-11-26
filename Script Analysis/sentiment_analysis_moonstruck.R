library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

moon <- paste(readLines("./Script Analysis/MoonstruckScript.html"))

moon <- data.frame(moon, stringsAsFactors = FALSE) %>%
  filter(!moon %in% c("", "</br>"))

tidy_moon <- moon %>%
  unnest_tokens(word, moon) %>%
  filter(word != "br")

moon_sentiment <- tidy_moon %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

write_rds(moon_sentiment, "nicolas_cage_analysis/moon_sentiment.rds")

top_moon <- moon_sentiment %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

top_moon %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

moon_sentiment2 <- tidy_moon %>%
  inner_join(get_sentiments("afinn")) %>%
  arrange(score) %>%
  mutate(average = mean(score))

write_rds(moon_sentiment2, "nicolas_cage_analysis/moon_sentiment2.rds")

moon_sentiment2 %>%
  ggplot(aes(x = score)) + geom_bar(fill = "skyblue")

moon_plot <- tidy_moon %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(row = row_number()) %>%
  count(index =  row %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

write_rds(moon_plot, "nicolas_cage_analysis/moon_plot.rds")

moon_plot %>%
  ggplot(aes(index, sentiment, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE)

