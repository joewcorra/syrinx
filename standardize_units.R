# FUNCTION: COVERT UNITS-------------------------------

# Function: covert units
convert_units <- function(data, unit1, unit2) {
  
  # Data should be a vector
  
  # Conversion factors
  conversion_factor <- case_when(
    unit1 == "" & unit2 == "" ~ , 
    unit1 == "" & unit2 == "" ~ ,
    unit1 == "" & unit2 == "" ~ ,
    unit1 == "" & unit2 == "" ~ ,
    .default = data
  )
  
  
  data <- data * conversion_factor
  
  return(data)
  
}