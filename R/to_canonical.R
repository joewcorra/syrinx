# =============================================================================
  # syrinx — to_canonical() and to_canonical_all()
  # Ingestion-layer unit conversion functions
  # ===========================================================================


# --- to_canonical ------------------------------------------------------------
# Converts a single column from a raw unit to its canonical unit by looking
# up the conversion factor in canonical_units. Errors if the unit is not found.
# Returns the modified dataframe (pipeline-friendly).

to_canonical <- function(df, col, from_unit) {

  # Get data dictionary
  units_path <- system.file("extdata",
                           "canonical_units.csv",
                           package = "syrinx")
  canonical_units <- readr::read_csv(units_path, show_col_types = FALSE)

  col_name <- rlang::as_name(rlang::enquo(col))

  # look up conversion factor — error hard on unrecognized unit
  lookup <- canonical_units %>%
    dplyr::filter(unit_raw == from_unit)

  if (nrow(lookup) == 0) {
    cli::cli_abort(c(
      "Unrecognized unit: {.val {from_unit}}",
      "i" = "Check {.code syrinx::canonical_units} for supported unit strings.",
      "i" = "If this unit is missing, add it to {.code canonical_units} before ingesting."
    ))
  }

  factor <- lookup %>% dplyr::pull(conversion_factor)
  canonical <- lookup %>% dplyr::pull(unit_canonical)

  cli::cli_inform(c(
    "v" = "Converting {.val {col_name}}: {.val {from_unit}} \u2192 {.val {canonical}} (x {factor})"
  ))

  df <- df %>%
    dplyr::mutate({{ col }} := {{ col }} * factor)

  return(df)

}


# --- to_canonical_all --------------------------------------------------------
# Batch version of to_canonical(). Accepts a named list mapping column names
# (as strings) to their raw unit strings, and converts all columns in one call.
# Returns the modified dataframe.
#
# Usage:
# df %>%
# to_canonical_all(unit_map = list(
# production_qty = "short_tons",
# heat_content = "mmbtu",
# fuel_consumption = "thousand_barrels"
# ))

to_canonical_all <- function(df, unit_map) {

  if (!is.list(unit_map) || is.null(names(unit_map))) {
    cli::cli_abort(c(
      "{.arg unit_map} must be a named list.",
      "i" = 'Example: {.code list(production_qty = "short_tons", heat_content = "mmbtu")}'
    ))
  }

  result <- purrr::reduce2(
    .x = names(unit_map),
    .y = unit_map,
    .f = function(df_acc, col_name, from_unit) {
      to_canonical(df_acc, col = !!rlang::sym(col_name), from_unit = from_unit)
    },
    .init = df
  )

  return(result)

}
