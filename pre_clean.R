library(dplyr)
library(stringr)
library(janitor)

pre_clean <- function(df) {
  
cleaned_df <- janitor::clean_names(df) %>%
  janitor::remove_empty(which = c("rows", "cols"), 
                        cutoff = 1, quiet = FALSE) %>%
  dplyr::mutate(dplyr::across(where(is.character), 
                ~stringr::str_squish() %>%
                  # Option: should we keep this?
                  # str_remove_all("[:punct:]"))  %>%
                  stringr::str_to_lower()
  )
)

get_dupes(cleaned_df)

return(cleaned_df)

}

# FUNCTION: STANDARDIZE COLUMN NAMES-----------------------------
# Read dictionary
# ghgi_variables <- read_csv("data/data_dictionary_variables.csv")

# harmonize_variables <- function(data, ghgi_variables) {
#   
#   # INCOMPLETE--need to fix the second regex below
#   
#   possible_matches <- colnames(data) %>%
#     as_tibble() %>%
#     rename(original_column_name = value) %>%
#     mutate(working_name = original_column_name %>%
#              str_squish() %>%
#              str_to_lower() %>%
#              str_remove_all("[:punct:]")) %>%
#     mutate(match = if_else(original_column_name %in% ghgi_variables$variable,
#                            TRUE,
#                            FALSE))
#   
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
