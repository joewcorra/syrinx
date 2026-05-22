#' Convert a column from a raw unit to its canonical unit
#'
#' Looks up the conversion factor for \code{from_unit} in
#' \code{canonical_units.csv} and multiplies the specified column by that
#' factor. Errors if the unit string is not found in the lookup table.
#' Pipeline-friendly: returns the modified data frame.
#'
#' @param df A data frame or tibble.
#' @param col <[`data-masked`][rlang::args_data_masking]> Column to convert
#'   (unquoted name).
#' @param from_unit A character string giving the raw unit for the column
#'   (e.g. \code{"short_tons"}, \code{"mmbtu"}).
#'
#' @return \code{df} with \code{col} multiplied by the appropriate conversion
#'   factor. Prints the conversion applied to the console.
#'
#' @examples
#' # df |> to_canonical(production_qty, "short_tons")
#'
#' @seealso \code{\link{to_canonical_all}}
#'
#' @importFrom readr read_csv
#' @importFrom rlang enquo as_name sym
#' @importFrom dplyr filter pull mutate
#'
#' @export
to_canonical <- function(df, col, from_unit) {

  units_path <- system.file("extdata",
                            "canonical_units.csv",
                            package = "syrinx")
  canonical_units <- readr::read_csv(units_path, show_col_types = FALSE)

  col_name <- rlang::as_name(rlang::enquo(col))

  lookup <- canonical_units |>
    dplyr::filter(unit_raw == from_unit)

  if (nrow(lookup) == 0) {
    stop(paste0(
      "Unrecognized unit: '", from_unit, "'. ",
      "Check canonical_units.csv for supported unit strings."
    ))
  }

  factor    <- dplyr::pull(lookup, conversion_factor)
  canonical <- dplyr::pull(lookup, unit_canonical)

  message(paste0("Converting '", col_name, "': ",
                 from_unit, " -> ", canonical, " (x ", factor, ")"))

  df <- df |>
    dplyr::mutate({{ col }} := {{ col }} * factor)

  return(df)

}


#' Batch-convert columns from raw units to canonical units
#'
#' Calls \code{\link{to_canonical}} once per entry in \code{unit_map},
#' converting each named column from its raw unit to its canonical unit.
#' Pipeline-friendly: returns the modified data frame.
#'
#' @param df A data frame or tibble.
#' @param unit_map A named list mapping column names (as character strings) to
#'   their raw unit strings. Example:
#'   \code{list(production_qty = "short_tons", heat_content = "mmbtu")}.
#'
#' @return \code{df} with all specified columns converted to canonical units.
#'
#' @examples
#' # df |>
#' #   to_canonical_all(unit_map = list(
#' #     production_qty   = "short_tons",
#' #     heat_content     = "mmbtu",
#' #     fuel_consumption = "thousand_barrels"
#' #   ))
#'
#' @seealso \code{\link{to_canonical}}
#'
#' @importFrom purrr reduce2
#' @importFrom rlang sym
#'
#' @export
to_canonical_all <- function(df, unit_map) {

  if (!is.list(unit_map) || is.null(names(unit_map))) {
    stop('unit_map must be a named list. ',
         'Example: list(production_qty = "short_tons", heat_content = "mmbtu")')
  }

  result <- purrr::reduce2(
    .x    = names(unit_map),
    .y    = unit_map,
    .f    = function(df_acc, col_name, from_unit) {
      to_canonical(df_acc, col = !!rlang::sym(col_name), from_unit = from_unit)
    },
    .init = df
  )

  return(result)

}
