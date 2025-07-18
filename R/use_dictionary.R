# Returns df of invalid values without changing anything

library(dplyr)
library(tidyr)

#' Compare a dataframe to the data dictionary
#'
#' @param df A dataframe to check.
#' @param type Either "columns" or "values". Determines what kind of discrepancy to identify.
#' @return A tibble or list of discrepancies (depends on `type`).
#' @export

use_dictionary <- function(df, type = c("columns", "values")) {

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
    # Check column name discrepancies
    dict_cols <- columns
    df_cols <- names(df)

    return(
      tibble::tibble(your_column = names(df)) %>%
        dplyr::if_else(result = dplyr::case_when(
          your_column %in% names(dict_cols) ~ "good",
          .default = "column name not in dictionary",
        )
      )
      )

  } else if (type == "values") {
    # Check value discrepancies
    dict_vals <- values

    discrepancies <- df %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "value_variable",
                          values_to = "value") %>%
      dplyr::anti_join(dict_vals)

    return(discrepancies)
}
}
