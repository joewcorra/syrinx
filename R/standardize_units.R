# FUNCTION: COVERT UNITS-------------------------------

# Need to identify value & units columns, or create if they don't exist

# Here's a way it might work:
harmonize_column_names <- function(colnames) {
  purrr::map_df(colnames, ~{
    ellmer::ask_ellmer(
      paste0("Standardize this measurement column name by splitting it into 'value' and 'units': '", .x, "'"),
      extract_json = TRUE
    )
  })
}


# Function: covert units
# convert_units <- function(data, unit1, unit2) {

  # Data should be a vector

  # Conversion factors
  # conversion_factor <- case_when(
  #   unit1 == "" & unit2 == "" ~ ,
  #   unit1 == "" & unit2 == "" ~ ,
  #   unit1 == "" & unit2 == "" ~ ,
  #   unit1 == "" & unit2 == "" ~ ,
  #   .default = data
  # )
  #
  #
  # data <- data * conversion_factor
  #
  # return(data)

# }
