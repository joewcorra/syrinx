library(labelled)
library(readr)
library(dplyr)
library(tibble)


# FUNCTION: APPLY ATTRIBUTE LABELS-------------------------------

# Function: add variable labels to dataframes
apply_labels <- function(df, type = c("columns", "values")) {

  # Get data dictionary
  vars_path <- system.file("data",
                           "data_dictionary_variables.csv",
                           package = "tanagerharmonize")
  vals_path <- system.file("data",
                           "data_dictionary_values.csv",
                           package = "tanagerharmonize")

  columns <- readr::read_csv(vars_path,
                             show_col_types = FALSE)
  values <- readr::read_csv(vals_path,
                            show_col_types = FALSE)

  type <- match.arg(type)

  if (!is.data.frame(df)) {
    stop("`df` must be a dataframe.")
  }

  if (type == "columns") {

    # Filter the variables dataframe to get the variable labels
    labels <- columns %>%
      dplyr::select(variable, variable_description) %>%
      dplyr::filter(variable %in%
               names(df)) %>%
      tibble::deframe() %>%
      as.list()

    # Apply the column labels
    labelled_df <- labelled::set_variable_labels(df, .labels = labels)

    return(labelled_df)

  } else if (type == "values") {

    labels <- values %>%
      dplyr::select(value, value_description) %>%

      tibble::deframe() %>%
      as.list()

    labelled_df <- labelled::set_value_labels(df, .labels = labels)

    return(labelled_df)
  }
}
