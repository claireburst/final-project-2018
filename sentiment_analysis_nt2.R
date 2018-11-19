library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)

nt2 <- paste(readLines("NationalTreasure2Script.htm"))

nt2 <- data.frame(nt2, stringsAsFactors = FALSE) %>%
  filter(nt2 != "")

tidy_nt2 <- nt2 %>%
  unnest_tokens(word, nt2)
