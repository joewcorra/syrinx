library(labelled)
library(readr)
library(dplyr)
library(magrittr)
library(tibble)

# FUNCTION: READ DATA DICTIONARY---------------------------------

ghgi_variables <- read_csv("data/data_dictionary_variables.csv")

ghgi_values <- read_csv("data/data_dictionary_values.csv")


# FUNCTION: APPLY ATTRIBUTE LABELS-------------------------------

# Function: add variable labels to dataframes
apply_variable_labels <- function(data) {



  return(data)

}

apply_labels <- function(df, type = c("columns", "values")) {

  type <- match.arg(type)

  if (!is.data.frame(df)) {
    stop("`df` must be a dataframe.")
  }

  if (type == "columns") {

    # Filter the variables dataframe to get the variable labels
    labels <- ghgi_variables %>%
      dplyr::select(variable, variable_description) %>%
      dplyr::filter(variable %in%
               names(df)) %>%
      tibble::deframe() %>%
      as.list()

    # Apply the column labels
    labelled_df <- labelled::set_variable_labels(df, .labels = labels)

    return(labelled_df)

  } else if (type == "values") {

    labels <- ghgi_values %>%
      dplyr::select(value, value_description) %>%

      tibble::deframe() %>%
      as.list()

    labelled_df <- labelled::set_value_labels(df, .labels = labels)

    return(labelled_df)
  }
}
