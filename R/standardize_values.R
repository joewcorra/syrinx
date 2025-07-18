library(dplyr)
library(purrr)
library(tidyr)
library(readr)

# FUNCTION: DATA VALIDATION--------------------------------------

# For character data only

validate_values <- function(data, ghgi_values, ghgi_variables) {

  filtering_data <- ghgi_values %>%
    filter(value_variable %in% colnames(data)) %>%
    pivot_wider(names_from = value_variable, values_from = value)

  invalid_data <- data %>%
    # Add a row number for id purposes when cross-referencing Excel sheets
    mutate(rownumber = row_number()) %>%
    mutate(across(matches(colnames(filtering_data)),
                  ~ if_else(. %in% pull(filtering_data, cur_column()),
                            .,
                            paste0(., "_INVALID_"))))

  # Filter to return only rows where something is invalid
  invalid_data <- invalid_data %>%
    filter(if_any(everything(), ~str_detect(., "_INVALID_")))

  return(invalid_data)

}


