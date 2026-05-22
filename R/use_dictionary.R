
#' Validate a data frame against the syrinx data dictionary
#'
#' Checks column names against \code{data_dictionary_variables.csv} and, for
#' columns that have value rules, checks every distinct value against
#' \code{data_dictionary_values.csv}. Returns a tidy table of discrepancies
#' that can be passed directly to \code{\link{llm_dictionary}} for
#' LLM-assisted correction.
#'
#' @param df A data frame or tibble to validate.
#'
#' @return A tibble with columns \code{column}, \code{value}, and
#'   \code{issue_type}. Each row is one discrepancy; \code{issue_type} is
#'   either \code{"column_not_in_dictionary"} or
#'   \code{"value_not_in_dictionary"}. Returns zero rows if no issues are
#'   found.
#'
#' @examples
#' # issues <- use_dictionary(my_df)
#' # if (nrow(issues) > 0) llm_dictionary(issues)
#'
#' @seealso \code{\link{llm_dictionary}}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr select filter mutate case_when all_of anti_join distinct bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#'
#' @export
use_dictionary <- function(df) {
  # Get data dictionary
  vars_path <- system.file("extdata",
                           "data_dictionary_variables.csv",
                           package = "syrinx")
  vals_path <- system.file("extdata",
                           "data_dictionary_values.csv",
                           package = "syrinx")
  columns <- readr::read_csv(vars_path, show_col_types = FALSE)
  values <- readr::read_csv(vals_path, show_col_types = FALSE)

  if (!is.data.frame(df)) {
    stop("df must be a dataframe.")
  }

  # Check column names
  dict_cols <- columns$variable
  col_check <- tibble::tibble(
    column = names(df),
    issue_type = dplyr::case_when(
      column %in% dict_cols ~ "ok",
      .default = "column_not_in_dictionary"
    )
  )

  # Check values for columns that ARE in the dictionary
  dict_vals <- values |>
    dplyr::select(value_variable, value)

  # Get columns that have value rules in the dictionary
  cols_with_rules <- unique(values$value_variable)

  # Columns in data that have validation rules
  cols_to_check <- intersect(names(df), cols_with_rules)

  if (length(cols_to_check) > 0) {
    val_check <- df |>
      dplyr::select(dplyr::all_of(cols_to_check)) |>
      tidyr::pivot_longer(cols = everything(),
                          names_to = "column",
                          values_to = "value") |>
      dplyr::distinct() |>
      dplyr::anti_join(dict_vals,
                       by = c("column" = "value_variable", "value" = "value")) |>
      dplyr::mutate(issue_type = "value_not_in_dictionary")
  } else {
    val_check <- tibble::tibble(
      column = character(),
      value = character(),
      issue_type = character()
    )
  }

  # Combine results
  # Add NA for value column in col_check to match structure
  col_issues <- col_check |>
    dplyr::filter(issue_type != "ok") |>
    dplyr::mutate(value = NA_character_) |>
    dplyr::select(column, value, issue_type)

  result <- dplyr::bind_rows(col_issues, val_check)

  return(result)
}
