

#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom ellmer chat_openai
#' @importFrom units
#'
# FUNCTION: CONVERT UNITS-------------------------------

# Consult the data dictionary for canonical units

# Identify units that the value is measured in
  # It might be in a 'units' column
  # It might be in the same column (e.g., 'coal_tbtu')
  # Somewhere else? in the metadata?

# Convert the value to the canonical units

# Report to user what happened

# Possible complications:
  # more than one value/unit per data frame
  # units are otherwise ambiguous or not indicated

# Should we embed the units in the metadata after conversion?

convert_units <- function(df, column, from_units) {

  # Get data dictionary
  vars_path <- system.file("extdata",
                           "data_dictionary_variables.csv",
                           package = "syrinx")

  # get the appropriate to_units from the dictionary

  # need list of all units used in {{units}} to do conversions
  # create new units not included in {{units}}
  # complete dictionary with canonical units

  # to_units <- look up from_units in dictionary and return "measure"
  # type e.g. mass, then return the appropriate canonical units



  # Convert string → units object
  from_units <- as_units(from_units)
  to_units <- as_units(to_units)


  df <- df |>
    mutate({{ column }} :=
             set_units(set_units({{ column }},
                                 from_units, mode = "standard"),
                       to_units,
                       mode = "standard"))

  return(df)
}
