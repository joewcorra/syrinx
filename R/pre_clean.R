#' Pre-clean a data frame
#'
#' Standardizes column names, removes empty rows/columns, squishes whitespace and
#' lowercases character columns, and converts any column matching "year" to a factor.
#' Also reports duplicate rows (without removing them).
#'
#' @details
#' The cleaning steps are:
#' \enumerate{
#'   \item \strong{Column names:} \code{janitor::clean_names()} converts names to \emph{snake_case}.
#'   \item \strong{Empty rows/cols:} \code{janitor::remove_empty()} drops rows/columns with no data.
#'   \item \strong{Character columns:} All character columns are trimmed with
#'         \code{stringr::str_squish()} and lowercased via \code{stringr::str_to_lower()}.
#'   \item \strong{Year columns:} Any column matching \code{"year"} is coerced to character,
#'         parsed with \code{readr::parse_number()} to extract the numeric year, then converted
#'         to a factor with \code{forcats::as_factor()}.
#'   \item \strong{Duplicates:} \code{janitor::get_dupes()} is called to \emph{report} duplicates
#'         to the console; duplicates are \emph{not} removed from the return value.
#' }
#'
#' @param df A data frame or tibble to be cleaned.
#'
#' @return A cleaned data frame with snake_case column names, empty rows/columns removed,
#'   character columns trimmed and lowercased, and any "year" column converted to a factor.
#'   Duplicate rows (if any) are printed to the console during execution.
#'
#' @examples
#' df <- data.frame(
#'   Year  = c("2020", " 2021 ", "2020"),
#'   Name  = c(" Alice ", "Bob", " Alice "),
#'   Value = c(1, 2, 1),
#'   stringsAsFactors = FALSE
#' )
#' pre_clean(df)
#'
#' @seealso \code{\link[janitor]{clean_names}}, \code{\link[janitor]{remove_empty}},
#'   \code{\link[janitor]{get_dupes}}, \code{\link[stringr]{str_squish}},
#'   \code{\link[stringr]{str_to_lower}}
#'
#' @keywords data-cleaning preprocessing
#'
#' @importFrom janitor clean_names remove_empty get_dupes
#' @importFrom dplyr mutate across where matches
#' @importFrom stringr str_squish str_to_lower
#' @importFrom readr parse_number
#' @importFrom forcats as_factor
#'
#' @export
pre_clean <- function(df) {

  cleaned_df <- janitor::clean_names(df) |>
    # remove any empty rows or columns
    janitor::remove_empty(which = c("rows", "cols"),
                          cutoff = 1, quiet = FALSE) |>
    # Squish char columns and make lowercase
    dplyr::mutate(dplyr::across(dplyr::where(is.character),
                                ~stringr::str_squish(.) |>
                                  # Option: should we keep this?
                                  # Don't want to remove decimals from #s stored as char
                                  # str_remove_all("[:punct:]"))  |>
                                  stringr::str_to_lower()),
                  # Convert 'year' (if present) to a factor
                  dplyr::across(dplyr::matches("year"),
                                # make year a character for parse_number()
                                ~as.character(.) |>
                                  # Pull the year as a number
                                  readr::parse_number() |>
                                  # Convert year to factor
                                  forcats::as_factor())
    )

  # look for dupes and return (do not remove)
  janitor::get_dupes(cleaned_df)

  return(cleaned_df)

}
