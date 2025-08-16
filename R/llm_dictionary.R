#' Suggest dictionary-conformant names via an LLM
#'
#' Uses a large language model (LLM) to map user-submitted column names or
#' values in a data frame to the closest valid entries defined in the package's
#' data dictionary. The function does not mutate \code{df}; it prepares the
#' invalid items and asks the model to return a plain-text table of suggested
#' matches.
#'
#' @details
#' The function reads two CSVs shipped with the package (located in
#' \code{inst/extdata} and accessed via \code{system.file()}):
#' \itemize{
#'   \item \strong{data_dictionary_variables.csv}: must contain a column
#'     \code{variable} listing allowed column names.
#'   \item \strong{data_dictionary_values.csv}: must contain columns
#'     \code{value_variable} (the owning column) and \code{value} (the allowed value).
#' }
#'
#' Behavior by \code{type}:
#' \describe{
#'   \item{\code{"columns"}}{Builds a vector of \emph{invalid} column names
#'         (those not in \code{variable}) and asks the LLM to map each to the
#'         most likely allowed name.}
#'   \item{\code{"values"}}{Keeps only character/factor columns, reshapes to
#'         long format (\code{value_variable}, \code{value}), de-duplicates, and
#'         anti-joins against the allowed values to obtain the set of
#'         \emph{invalid} values. The LLM is asked to map each invalid value to
#'         the closest allowed value and return a three-column table:
#'         \code{submitted_value}, \code{matched_value}, and \code{value_variable}.}
#' }
#'
#' \strong{Model call and output:} The function starts an \pkg{ellmer} chat via
#' \code{ellmer::chat_openai()} (default model \code{"gpt-4.1"}) and sends a
#' prompt instructing the model to return a plain-text table suitable for
#' display in the R console. The return value is the model response as provided
#' by \pkg{ellmer}.
#'
#' @section Security & configuration notes:
#' \itemize{
#'   \item Prefer supplying your OpenAI API key via the environment variable
#'         \code{OPENAI_API_KEY} (e.g., in \code{.Renviron}) rather than
#'         hard-coding. You may also pass \code{api_key} and forward it to
#'         \code{chat_openai(api_key = ...)}.
#'   \item Avoid embedding secrets in source control. Do not \code{Sys.setenv()}
#'         with live keys inside package code.
#'   \item For deterministic behavior and auditability, consider adding server-
#'         side validation and/or a manual review step downstream of the LLM suggestions.
#' }
#'
#' @param df A data frame (or tibble) containing user-submitted names/values to check.
#' @param type Either \code{"columns"} or \code{"values"} to select what the LLM
#'   should reconcile.
#' @param api_key Optional character scalar. OpenAI API key to use with
#'   \code{ellmer::chat_openai()}. If \code{NULL} or \code{NA}, the function
#'   relies on \code{Sys.getenv("OPENAI_API_KEY")}.
#'
#' @return The model response object returned by \pkg{ellmer} (typically a
#'   character string containing a plain-text table). No changes are made to \code{df}.
#'
#' @examples
#' \dontrun{
#' # Using an environment variable OPENAI_API_KEY is recommended.
#' # Columns mode
#' df <- tibble::tibble(Yeer = 2020, Sektor = "residential", value = 1)
#' out_cols <- llm_dictionary(df, type = "columns")
#' cat(out_cols)
#'
#' # Values mode
#' df2 <- tibble::tibble(sector = c("residental", "industrial"), value = c(1, 2))
#' out_vals <- llm_dictionary(df2, type = "values")
#' cat(out_vals)
#' }
#'
#' @seealso \code{\link[ellmer]{chat_openai}}, \code{\link[readr]{read_csv}},
#'   \code{\link[tidyr]{pivot_longer}}, \code{\link[dplyr]{anti_join}}
#'
#' @keywords data-cleaning validation dictionary llm
#'
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#' @importFrom purrr keep
#' @importFrom dplyr select distinct anti_join
#' @importFrom tidyr pivot_longer
#' @importFrom ellmer chat_openai
#'
#' @export
# NEED TO RESTRICT "values" to a single column at a time, perhaps
llm_dictionary <- function(df, type = c("columns", "values"), api_key = NA_character_) {

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
Sys.setenv(OPENAI_API_KEY = api_key)
# Start a chat with ChatGPT
chat <- chat_openai(
  api_key = Sys.getenv("OPENAI_API_KEY")
)

# Start a chat with ChatGPT
chat <- chat_openai(model = "gpt-4.1", api_key = Sys.getenv("OPENAI_API_KEY"))


# Construct a prompt
if (type == "columns") {
prompt <- paste0(
  "Match each of the following submitted column names to the most likely column name from this master list.  Please provide your answers as a plan text table (viewable in the R console) with two columns: submitted_column_name and matched_column_name. Do not output any explanations, only the table.",
  paste(dplyr::select(columns, variable), collapse = ", "),
  ".\n\nSubmitted column names:\n",
  paste(input_data, collapse = "\n")
)

} else if (type == "values") {
  prompt <- paste0(
    "Match each of the following submitted values in the 'value' column to the most likely value from this master list. Please provide your answers as a plan text table (viewable in the R console) with three columns: submitted_value, matched_value, and value_variable. Do not output any explanations, only the table.",
    paste(dplyr::select(values, value_variable:value), collapse = ", "),
    ".\n\nSubmitted values:\n",
    paste(input_data, collapse = "\n"))
}

# Ask the model
response <- chat$chat(prompt)

return(response)

}
