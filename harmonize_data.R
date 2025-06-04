library(tidyverse)
library(readxl)
library(labelled)

# FUNCTION: READ DATA DICTIONARY---------------------------------

ghgi_variables <- read_csv("data/data_dictionary_variables.csv")
  
ghgi_values <- read_csv("data/data_dictionary_values.csv")

# FUNCTION, VER. II: READ INVDB DIM FILES---------------------------------

# Creates one two-column df

read_dim_files <- function(folderpath) {
  
  dim_files <- list.files(folderpath, 
                          pattern = "\\.csv$",
                          full.names = TRUE)
  
  dim_file_list <- map(dim_files, read_csv) %>% 
    set_names(str_split_i(dim_files, "/", 3) %>% 
                str_remove_all(c("_|dim|Dim|National ER|Values|.csv|PY25")) %>% 
                str_squish())
  
  sectors <- left_join(dim_file_list$subsector,
                       dim_file_list$sector, 
                       by = "sector_id") %>%
    left_join(dim_file_list$category, by = "subsector_id") %>%
    select(-ends_with("_id")) %>%
    pivot_longer(cols = everything(), values_to = "value", names_to = "value_variable") %>%
    mutate(value_variable = str_extract(value_variable, pattern = "^[^_]+"), 
           value_description = "")
  
  ghg <- dim_file_list$ghg %>%
    select(value = ghg_code, value_description = ghg_longname) %>%
    mutate(value_variable = "ghg")
  
  carbon_pool <- dim_file_list$carbonpool %>%
    rename(value = carbon_pool) %>%
    mutate(value_description = "", 
           value_variable = "carbon_pool")
  
  subcats1 <- dim_file_list$subcategory1 %>% 
    mutate(value_description = "category-1", 
           value_variable = "subcategory") %>% 
    rename(value = 1)
  subcats2 <- dim_file_list$subcategory2 %>% 
    mutate(value_description = "category-2", 
           value_variable = "subcategory") %>% 
    rename(value = 1)
  subcats3 <- dim_file_list$subcategory3 %>% 
    mutate(value_description = "category-3",
           value_variable = "subcategory") %>% 
    rename(value = 1)
  
  subcats <- bind_rows(subcats1, subcats2, subcats3)
  
  invdb <- lst(sectors, 
               ghg, 
               carbon_pool, 
               subcats) %>%
    map(\(x) mutate(x, 
                    across(where(is.character), 
                           ~str_to_lower(.)))) %>%
    list_rbind()
  
  return(invdb)
  
}


folderpath <- "data/InvDB Dim Values PY25 National ER"
invdb <- read_dim_files(folderpath)


# FUNCTION: STANDARDIZE COLUMN NAMES-----------------------------

harmonize_variables <- function(data, ghgi_variables) {

# INCOMPLETE--need to fix the second regex below
  
  possible_matches <- colnames(data) %>%
    as_tibble() %>%
    rename(original_column_name = value) %>%
    mutate(working_name = original_column_name %>%
             str_squish() %>%
             str_to_lower() %>%
             str_remove_all("[:punct:]")) %>%
    mutate(match = if_else(original_column_name %in% ghgi_variables$variable,
                           TRUE,
                           FALSE))
  
  possible_matches <- possible_matches %>%
    mutate(suggestions = if_else(match == FALSE, map_chr(working_name, ~ghgi_variables %>% 
                                   filter(stringdist(aliases, .x, method = "lcs") < 6) %>% 
                                   pull(variable) %>% 
                                   str_flatten_comma()), 
                                 NA_character_)) %>% 
    select(original_column_name, match, suggestions)
  
  return(possible_matches)
  
}

# FUNCTION: DATA VALIDATION--------------------------------------

# For character data only

validate_values <- function(data, ghgi_values, ghgi_variables) {
  
  filtering_data <- ghgi_values %>%
    filter(value_variable %in% colnames(data)) %>%
    pivot_wider(names_from = value_variable, values_from = value)
  
invalid_data <- data %>%
  # Add a row number for id purposes when cross-referencing Excel sheets
  mutate(rownumber = row_number()) %>%
  mutate(across(matches(colnames(filtering_data)), 
         ~ if_else(. %in% pull(filtering_data, cur_column()), 
                   ., 
                   paste0(., "_INVALID_"))))

# Filter to return only rows where something is invalid
invalid_data <- invalid_data %>%
filter(if_any(everything(), ~str_detect(., "_INVALID_")))

return(invalid_data)

}


# FUNCTION: APPLY ATTRIBUTE LABELS-------------------------------

# Function: add variable labels to dataframes 
apply_variable_labels <- function(data, ghgi_variables) {
  
  # Filter the variables dataframe to get the variable labels
  labels <- deframe(ghgi_variables %>%
                      select(-variable_type) %>% 
                      filter(variable %in%
                               colnames(data))) %>%
    as.list()

# Apply the column labels
var_label(data) <- labels

  return(data)

}


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

# DEMO: STANDARDIZE VARIABLE NAMES---------------------

mydata <- tibble(
  Sector = c("industrial", "residential", "commercial"), 
  'greenhouse gas' = c("CO_2", "CO_2", "CO_2"), 
  eValue = c(100, 200, 300), 
  US_State = c("MD", "VA", "NY")
)

mydata

harmonize_variables(mydata, ghgi_variables)
# This isn't really working that well--ideas?


# DEMO: INTEGRATE METADATA INTO TABLE---------------------

mydata <- tibble(
  econ_sector = c("industrial", "residential sector", "commercial", "commercial", "commercal"), 
  ghg = c("co2", "carbon dioxide", "co.2","co2", "co2"), 
  value = c(100, 200, 300, 250, 200), 
  state = c("md", "Va.", "NY", "me", "D E")
)

mydata

results <- apply_variable_labels(mydata, ghgi_variables)

view(results)
labelled::get_variable_labels(results)


# DEMO: VALIDATE VALUES---------------------


validate_values(mydata, ghgi_values)
# Alternative: a table with highlighted invalid values for visual review

library(gt)
validate_values(mydata, ghgi_values) %>% 
  gt() %>%
  text_transform(locations = cells_body(columns = everything()),
                 fn = \(.x) {
                   str_replace(.x, "^(.*)_INVALID_.*$", 
                               paste0('<a style = "color:red">', .x, '</a>')) %>%
                     str_remove("_INVALID_")
                 })

