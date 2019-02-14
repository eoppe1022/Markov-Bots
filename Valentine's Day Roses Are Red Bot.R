
library(tidyverse)
library(rvest)
library(markovifyR) #devtools::install_github("abresler/markovifyR")

## Webpage 1
page_1 <- read_html("https://www.waterstones.com/blog/roses-are-red")

poems_1 <- page_1 %>%
  html_nodes(".clearfix p") %>% 
  html_text() %>% 
  enframe(name = NULL) %>% 
  filter(str_detect(value, "^Roses are red")) %>%
  filter(!c(row_number() %in% c(31, 28, 25, 15, 7)))

## Webpage 2
page_2 <- read_html("http://www.beautiful-love-quotes.com/roses-are-red-poems.html")

poems_2 <- page_2 %>%
  html_nodes("br+ p") %>%
  html_text() %>%
  enframe(name = NULL) %>%
  mutate(value = str_replace_all(value, c("\r\n" = " ", "\U007E\\s\\sKYB" = ""))) %>%
  mutate(value = str_squish(value)) %>%
  filter(str_detect(value, "^Roses are red")) %>%
  filter(row_number() != 11)

## Webpage 3
page_3 <- read_html("https://www.scoopify.org/roses-are-red-violets-are-blue-poems/")

poems_3 <- page_3 %>%
  html_nodes("hr+ p") %>%
  html_text() %>%
  enframe(name = NULL) %>%
  mutate(value = str_replace_all(value, "\n", ", ")) %>%
  filter(str_detect(value, "^Roses are red"))

## Webpage 4
page_4 <- read_html("https://top-funny-jokes.com/roses-are-red-violets-are-blue-jokes/")

poems_4 <- page_4 %>%
  html_nodes("#the-post li") %>%
  html_text() %>%
  enframe(name = NULL) %>%
  mutate(value = str_squish(value)) %>%
  filter(str_detect(value, "^Roses are red"))

## Combine them
poems <- bind_rows(poems_1, poems_2, poems_3, poems_4) %>%
  mutate(value = tolower(value)) %>%
  mutate(value = str_replace(value, "roses\\sare\\sred\\s", "roses are red, ")) %>%
  mutate(value = str_replace(value, ",violets", ", violets")) %>%
  mutate(value = str_replace_all(value, ",,", ",")) %>%
  mutate(value = str_replace_all(value, "violents", "violets")) %>%
  mutate(value = str_replace_all(value, c("\\," = "", "\\." = "", "\\?" = "", "\\!" = ""))) %>%
  rename(poem = value)

## The bot
bot <- poems %>% 
  pull(poem) %>% 
  generate_markovify_model(markov_state_size = 1) %>%
  markovify_text(maximum_sentence_length = NULL, count = 250, tries = 2000, only_distinct = TRUE, return_message = FALSE) %>%
  purrr::set_names("id", "poem") %>%
  select(poem) %>%
  filter(str_detect(poem, "^roses are red violets are blue"))

bot
