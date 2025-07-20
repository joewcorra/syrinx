library(dplyr)
library(stringr)
library(janitor)
library(readr)
library(forcats)
library(magrittr)
#' pre_clean
#'
#' Pre cleans data
#' @param ... describe params
#' @return describe return
#' @export


pre_clean <- function(df) {

  cleaned_df <- janitor::clean_names(df) %>%
    # remove any empty rows or columns
    janitor::remove_empty(which = c("rows", "cols"),
                          cutoff = 1, quiet = FALSE) %>%
    # Squish char columns and make lowercase
    dplyr::mutate(dplyr::across(dplyr::where(is.character),
                                ~stringr::str_squish(.) %>%
                                  # Option: should we keep this?
                                  # Don't want to remove decimals from #s stored as char
                                  # str_remove_all("[:punct:]"))  %>%
                                  stringr::str_to_lower()),
                  # Convert 'year' (if present) to a factor
                  dplyr::across(dplyr::matches("year"),
                                # make year a character for parse_number()
                                ~as.character(.) %>%
                                  # Pull the year as a number
                                  readr::parse_number() %>%
                                  # Convert year to factor
                                  forcats::as_factor())
    )

  # look for dupes and return (do not remove)
  janitor::get_dupes(cleaned_df)

  return(cleaned_df)

}
