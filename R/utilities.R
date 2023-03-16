



#' Check for label mismatches across years
#'
#' Examine multiple years for an ACS table and find instances where the label for
#' a line in one year does not match the label for the same line in another year.
#'
#' @param table table name as character string
#' @param years vector of years to examine
#'
#' @return tibble with line, year, variable and label where mismatches occur
#' @export
#'
#' @examples
#' check_var_mismatch("B27003", c(2016, 2021))
#' check_var_mismatch("B27003", c(2011, 2016, 2021))
check_var_mismatch <- function(table, years){
  table <- stringr::str_to_upper(table)

  basedf <- bacs::acsvars |>
    filter(table==!!table, year %in% !!years) |>
    select(year, line, variable, label) |>
    arrange(year, line)

  nyears_sought <- length(years)
  nyears_available <- nrow(count(basedf, year))
  if(nyears_available < nyears_sought){
    print("Number of years available < number desired.")
    print("Reporting on years available.")
  }

  # identify lines where labels differ across years

  line_mismatches <- basedf |>
    summarise(n=n(), .by=c(line, label)) |>
    filter(n < nyears_available) |>
    select(line) |>
    distinct() |>
    pull(line)

  basedf |>
    filter(line %in% line_mismatches) |>
    arrange(line, year) |>
    select(line, year, variable, label)
}

# check_var_mismatch("B27003", c(2011, 2016, 2021))
#
# years <- c(2011, 2016, 2021)
