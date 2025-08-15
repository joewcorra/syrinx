#' Check a data frame against the package data dictionary
#'
#' Compares a data frame's column names or values to the package's data
#' dictionary and reports discrepancies without modifying the input.
#'
#' @details
#' This function reads two CSV files shipped with the package (under
#' \code{inst/extdata} and accessed via \code{system.file()}):
#' \itemize{
#'   \item \strong{data_dictionary_variables.csv} — must contain a column
#'         \code{variable} listing all allowed column names.
#'   \item \strong{data_dictionary_values.csv} — must contain columns
#'         \code{value_variable} (the column name the value belongs to) and
#'         \code{value} (the allowed value).
#' }
#'
#' Behavior by \code{type}:
#' \describe{
#'   \item{\code{"columns"}}{Returns a tibble with each of \code{df}'s column
#'   names and a \code{result} indicating whether that name exists in the
#'   dictionary.}
#'   \item{\code{"values"}}{Selects columns of \code{df} whose names match any
#'   \code{value_variable} in the value dictionary, collapses to distinct
#'   pairs \code{(value_variable, value)}, and returns the subset whose values
#'   are \emph{not} listed as allowed in the dictionary.}
#' }
#'
#' @param df A data frame (or tibble) to check.
#' @param type Either \code{"columns"} or \code{"values"} to choose which kind
#'   of discrepancy to identify. Partial matching is not allowed.
#'
#' @return
#' A tibble of discrepancies:
#' \itemize{
#'   \item For \code{type = "columns"}: a tibble with columns
#'         \code{your_column} and \code{result} (\code{"good"} or a message).
#'   \item For \code{type = "values"}: a tibble with columns
#'         \code{value_variable} and \code{value} listing values present in
#'         \code{df} but not found in the dictionary for that variable.
#' }
#'
#' @examples
#' \dontrun{
#' # Suppose the dictionary lists allowed columns: c("year", "sector", "value")
#' # and allowed values for sector: c("residential", "commercial")
#' library(tibble)
#' df <- tibble(Year = c(2020, 2021), sector = c("Residential", "industrial"), value = 1:2)
#'
#' # Column-name check
#' use_dictionary(df, type = "columns")
#'
#' # Value check (reports values not in the dictionary per value_variable)
#' use_dictionary(df, type = "values")
#' }
#'
#' @seealso \code{\link[readr]{read_csv}}, \code{\link[tidyr]{pivot_longer}},
#'   \code{\link[dplyr]{anti_join}}
#'
#' @keywords data-cleaning validation dictionary
#'
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when select distinct matches anti_join
#' @importFrom tidyr pivot_longer
#'
#' @export

library(dplyr)
library(tidyr)

#' use_dictionary
#' Compare a dataframe to the data dictionary
#'
#' @param df A dataframe to check.
#' @param type Either "columns" or "values". Determines what kind of discrepancy to identify.
#' @return A tibble or list of discrepancies (depends on `type`).
#' @export

use_dictionary <- function(df, type = c("columns", "values")) {

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
    # Check column name discrepancies
    dict_cols <- columns$variable

    return(
      tibble::tibble(your_column = names(df)) |>
        dplyr::mutate(result = dplyr::case_when(
          your_column %in% dict_cols ~ "good",
          .default = "column name not in dictionary",
        )
      )
      )

  } else if (type == "values") {
    # Check value discrepancies
    dict_vals <- values |> dplyr::select(value_variable, value)

    discrepancies <- df |>
      dplyr::select(matches(values$value_variable)) |>
      dplyr::distinct() |>
      tidyr::pivot_longer(cols = everything(),
                          names_to = "value_variable",
                          values_to = "value") |>
      dplyr::anti_join(dict_vals)

    return(discrepancies)
}
}
