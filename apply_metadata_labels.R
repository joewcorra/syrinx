library(labelled)


# FUNCTION: READ DATA DICTIONARY---------------------------------

ghgi_variables <- read_csv("data/data_dictionary_variables.csv")

ghgi_values <- read_csv("data/data_dictionary_values.csv")


# FUNCTION: APPLY ATTRIBUTE LABELS-------------------------------

# Function: add variable labels to dataframes 
apply_variable_labels <- function(data, ghgi_variables) {
  
  # Filter the variables dataframe to get the variable labels
  labels <- deframe(ghgi_variables %>%
                      select(-variable_type) %>% 
                      filter(variable %in%
                               colnames(data))) %>%
    as.list()
  
  # Apply the column labels
  var_label(data) <- labels
  
  return(data)
  
}