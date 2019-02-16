library(tidyverse)
library(rtweet)
library(markovifyR) #devtools::install_github("abresler/markovifyR")

## Get tweets with "rtweet"
## FYI you need to set up "rtweet" beforehand

ombudsman_data <- get_timeline("@CanesOmbudsman", n = 10000)

## Take out RTs and replies and create Markov model

bot_markov_model <- ombudsman_data %>%
  filter(is_retweet == FALSE) %>%
  filter(is.na(reply_to_user_id)) %>%
  pull(text) %>%
  generate_markovify_model(markov_state_size = 1) %>%
  markovify_text(maximum_sentence_length = NULL, count = 250, tries = 2000, only_distinct = TRUE, return_message = FALSE) %>%
  set_names("id", "tweet")
  
## Print output

bot_markov_model %>%
  print(n = 25)
