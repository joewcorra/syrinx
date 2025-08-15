#' Apply variable (column) labels from the data dictionary
#'
#' Adds human-readable variable labels to the columns of a data frame using the
#' package's data dictionary, without modifying data values or column names.
#'
#' @details
#' This function reads \code{inst/extdata/data_dictionary_variables.csv}
#' (accessed via \code{system.file()}) and expects at least the columns:
#' \itemize{
#'   \item \code{variable}: the canonical column name in \code{df}
#'   \item \code{variable_description}: the label to apply
#' }
#' Only variables present in \code{df} are labeled. Labels are stored as
#' variable attributes via \code{labelled::set_variable_labels()} and are
#' compatible with packages that respect the \code{label} attribute.
#'
#' \emph{Future extension:} Commented code shows a pattern for value labels
#' (via \code{labelled::set_value_labels()}) using a
#' \code{data_dictionary_values.csv} with fields like
#' \code{value_variable}, \code{value}, and \code{value_description}.
#'
#' @param df A data frame or tibble whose columns should receive labels.
#'
#' @return The input data frame with variable label attributes applied to any
#'   columns found in the dictionary.
#'
#' @examples
#' # Suppose the dictionary maps variable = "sector" to "Economic sector"
#' df <- tibble::tibble(sector = c("residential", "commercial"), value = 1:2)
#' out <- apply_labels(df)
#' # View a label (if your IDE prints attributes) or use:
#' attr(out$sector, "label")
#'
#' @seealso \code{\link[labelled]{set_variable_labels}}, \code{\link[readr]{read_csv}}
#'
#' @keywords metadata labelling documentation
#'
#' @importFrom readr read_csv
#' @importFrom dplyr select filter
#' @importFrom tibble deframe
#' @importFrom labelled set_variable_labels
#'
#' @export

library(labelled)
library(readr)
library(dplyr)
library(tibble)

apply_labels <- function(df
                         # ,
                         # type = c("columns", "values")
                         ) {

  # Get data dictionary
  vars_path <- system.file("extdata",
                           "data_dictionary_variables.csv",
                           package = "tanagerharmonize")
  # vals_path <- system.file("extdata",
  #                          "data_dictionary_values.csv",
  #                          package = "tanagerharmonize")

  columns <- readr::read_csv(vars_path,
                             show_col_types = FALSE)
  # values <- readr::read_csv(vals_path,
  #                           show_col_types = FALSE)

  # type <- match.arg(type)
#
#   if (!is.data.frame(df)) {
#     stop("`df` must be a dataframe.")
#   }
#
#   if (type == "columns") {

    # Filter the variables dataframe to get the variable labels
    labels <- columns |>
      dplyr::select(variable, variable_description) |>
      dplyr::filter(variable %in%
               names(df)) |>
      tibble::deframe() |>
      as.list()

    # Apply the column labels
    labelled_df <- labelled::set_variable_labels(df, .labels = labels)

    return(labelled_df)

  # } else if (type == "values") {
  #
  #   labels <- values |>
  #     dplyr::filter(value_variable %in% names(df)) |>
  #     dplyr::select(value, value_description) |>
  #     tibble::deframe() |>
  #     as.list()
  #
  #   labelled_df <- labelled::set_value_labels(df, .labels = labels,
  #                                             null_action = "empty")

    return(labelled_df)
  # }
}
