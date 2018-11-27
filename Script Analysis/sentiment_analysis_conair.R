library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

con <- paste(readLines("./Script Analysis/ConAirScript.html"))

con <- data.frame(con, stringsAsFactors = FALSE) %>%
  filter(!con %in% c("", "</br>", "<p>"))

tidy_con <- con %>%
  unnest_tokens(word, con) %>%
  filter(word != "br")

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
