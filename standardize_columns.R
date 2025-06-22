library(tidyverse)
library(readxl)

# FUNCTION: READ DATA DICTIONARY---------------------------------

ghgi_variables <- read_csv("data/data_dictionary_variables.csv")

ghgi_values <- read_csv("data/data_dictionary_values.csv")


# FUNCTION: STANDARDIZE COLUMN NAMES-----------------------------

harmonize_variables <- function(data, ghgi_variables) {
  
  # INCOMPLETE--need to fix the second regex below
  
  possible_matches <- colnames(data) %>%
    as_tibble() %>%
    rename(original_column_name = value) %>%
    mutate(working_name = original_column_name %>%
             str_squish() %>%
             str_to_lower() %>%
             str_remove_all("[:punct:]")) %>%
    mutate(match = if_else(original_column_name %in% ghgi_variables$variable,
                           TRUE,
                           FALSE))
  
  # Use stringdist to guess matches
  # possible_matches <- possible_matches %>%
  #   mutate(suggestions = if_else(match == FALSE, map_chr(working_name, ~ghgi_variables %>% 
  #                                  filter(stringdist(aliases, .x, method = "lcs") < 6) %>% 
  #                                  pull(variable) %>% 
  #                                  str_flatten_comma()), 
  #                                NA_character_)) %>% 
  #   select(original_column_name, match, suggestions)
  
  return(possible_matches)
  
}