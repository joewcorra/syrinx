
#' Validate a data frame against a data dictionary
#'
#' Checks column names against a variables dictionary and, for columns that
#' have value rules, checks every distinct value against a values dictionary.
#' Returns a tidy table of discrepancies that can be passed directly to
#' \code{\link{llm_dictionary}} for LLM-assisted correction.
#'
#' By default, both checks run against the package's built-in GHG inventory
#' data dictionaries. Either dictionary can be swapped out for a custom CSV,
#' and either check can be skipped entirely by passing \code{NULL} for the
#' corresponding argument.
#'
#' @param df A data frame or tibble to validate.
#' @param dictionary_variables Path to a CSV file used to validate column
#'   names. Must contain a \code{variable} column listing the allowed column
#'   names. Defaults to the package's built-in
#'   \code{data_dictionary_variables.csv}. Pass \code{NULL} to skip the
#'   column-name check entirely.
#' @param dictionary_values Path to a CSV file used to validate cell values.
#'   Must contain \code{value_variable} (the column the rule applies to) and
#'   \code{value} (an allowed value) columns. Defaults to the package's
#'   built-in \code{data_dictionary_values.csv}. Pass \code{NULL} to skip the
#'   value check entirely.
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
#' # Using a custom dictionary, checking only column names:
#' # issues <- use_dictionary(my_df,
#' #   dictionary_variables = "my_vars.csv",
#' #   dictionary_values = NULL
#' # )
#'
#' @seealso \code{\link{llm_dictionary}}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr select filter mutate case_when all_of anti_join distinct bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#'
#' @export
use_dictionary <- function(df,
                           dictionary_variables = system.file("extdata",
                                                              "data_dictionary_variables.csv",
                                                              package = "syrinx"),
                           dictionary_values = system.file("extdata",
                                                           "data_dictionary_values.csv",
                                                           package = "syrinx")) {

  if (!is.data.frame(df)) {
    stop("df must be a dataframe.")
  }

  # Check column names, unless the caller opted out
  if (!is.null(dictionary_variables)) {
    columns <- readr::read_csv(dictionary_variables, show_col_types = FALSE)

    if (!"variable" %in% names(columns)) {
      stop("dictionary_variables must contain a 'variable' column.")
    }

    dict_cols <- columns$variable
    col_check <- tibble::tibble(
      column = names(df),
      issue_type = dplyr::case_when(
        column %in% dict_cols ~ "ok",
        .default = "column_not_in_dictionary"
      )
    )

    col_issues <- col_check |>
      dplyr::filter(issue_type != "ok") |>
      dplyr::mutate(value = NA_character_) |>
      dplyr::select(column, value, issue_type)
  } else {
    col_issues <- tibble::tibble(
      column = character(),
      value = character(),
      issue_type = character()
    )
  }

  # Check values for columns that ARE in the dictionary, unless the caller
  # opted out
  if (!is.null(dictionary_values)) {
    values <- readr::read_csv(dictionary_values, show_col_types = FALSE)

    if (!all(c("value_variable", "value") %in% names(values))) {
      stop("dictionary_values must contain 'value_variable' and 'value' columns.")
    }

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
  } else {
    val_check <- tibble::tibble(
      column = character(),
      value = character(),
      issue_type = character()
    )
  }

  # Combine results
  result <- dplyr::bind_rows(col_issues, val_check)

  return(result)
}
