library(ellmer)
library(jsonlite)
library(dplyr)
library(tibble)
library(purrr)

#' llm_dictionary
#'
#'
#' @param df A dataframe to check.
#' @param type Either "columns" or "values". Determines what kind of discrepancy to identify.
#' @return A tibble or list of discrepancies (depends on `type`).
#' @export

# NEED TO RESTRICT "values" to a single column at a time...i think
llm_dictionary <- function(df, type = c("columns", "values", api_key = NA)) {

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

  input_data = dplyr::if_else(type == "columns", names(df),
                      purrr::keep(df, ~ is.character(.) || is.factor(.)) %>%
                        tibble::deframe()
                      )

# Enter API Key
Sys.setenv(OPENAI_API_KEY = "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")
# Start a chat with ChatGPT
chat <- chat_openai(
  api_key = Sys.getenv("OPENAI_API_KEY")
)

# Start a chat with ChatGPT
chat <- chat_openai(model = "gpt-4o", api_key = Sys.getenv("OPENAI_API_KEY"))

# Construct a prompt
prompt <- paste0(
  "Match each of the following submitted value names to the most likely value from this master list. Please provide your answers in a tabular format",
  paste(columns, collapse = ", "),
  ".\n\nSubmitted values:\n",
  paste(input_data, collapse = "\n")
)

# Ask the model
response <- chat$chat(prompt)

return(response)

}

# Standardizing units and values of measurements----------------------------
# Need to identify value & units columns, or create if they don't exist
# Here's a way it might work:

# harmonize_measurements <- function(df) {
#
#
#   num_df <- df %>%
#     select(where(is.numeric))
#
#   units_prompt <- paste0(
#     "Please look at these column names and identify that look like measurements of something. Please return your answer in tabular format only.",
#     colnames(num_df))
#
#   # Ask the model
#   response <- chat$chat(units_prompt)
# }
#
# y <- tibble(site = c("a", "b"), 'mmt co2' = 500:501, 'mmt ch4' = 101:102)
# harmonize_measurements(y)
