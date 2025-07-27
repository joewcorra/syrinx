library(ellmer)
library(jsonlite)
library(dplyr)
library(tibble)
library(purrr)
library(tidyr)

#' llm_dictionary
#'
#'
#' @param df A dataframe to check.
#' @param type Either "columns" or "values". Determines what kind of discrepancy to identify.
#' @return A tibble or list of discrepancies (depends on `type`).
#' @export

# NEED TO RESTRICT "values" to a single column at a time...i think
llm_dictionary <- function(df, type = c("columns", "values"), api_key = NA) {

  # Get data dictionary
  vars_path <- system.file("extdata",
                           "data_dictionary_variables.csv",
                           package = "tanagerharmonize")
  vals_path <- system.file("extdata",
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
    # Keep only invalid column mames
    input_data <- names(df)[!names(df) %in% columns$variable] |>
      tibble::as_tibble()

  } else if (type == "values") {
    # if values, keep only invalid values that are char data
    # NOTE: the assumption here is that user has already fixed the COLUMN NAMES
    input_data <- purrr::keep(df, ~ is.character(.) || is.factor(.)) |>
      tidyr::pivot_longer(cols = everything(),
                          names_to = "value_variable",
                          values_to = "value") |>
      dplyr::distinct() |>
      dplyr::anti_join(values |>
                         select(-value_description))
  }


# Enter API Key
Sys.setenv(OPENAI_API_KEY = "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")
# Start a chat with ChatGPT
chat <- chat_openai(
  api_key = Sys.getenv("OPENAI_API_KEY")
)

# Start a chat with ChatGPT
chat <- chat_openai(model = "gpt-4o", api_key = Sys.getenv("OPENAI_API_KEY"))


# Construct a prompt
if (type == "columns") {
prompt <- paste0(
  "Match each of the following submitted column names to the most likely column name from this master list. Please provide a succinct response in tabular format.
Please provide your answers in plain text table that may be viewed in the R console",
  paste(dplyr::select(columns, variable), collapse = ", "),
  ".\n\nSubmitted values:\n",
  paste(input_data, collapse = "\n")
)

} else if (type == "values") {
  prompt <- paste0(
    "Match each of the following submitted values in the 'value' column to the most likely value from this master list. Please provide your answers in plain text table that may be viewed in the R console",
    paste(dplyr::select(values, value_variable:value), collapse = ", "),
    ".\n\nSubmitted values:\n",
    paste(input_data, collapse = "\n"))
}

# Ask the model
response <- chat$chat(prompt)

return(response)

}
