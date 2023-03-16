
#' ACS tables metadata
#'
#' @source Constructed from Census documentation <https://www.census.gov/programs-surveys/acs/technical-documentation/table-shells.html>.
#'
#' @format
#' Tibble with 1 row per ACS table, with columns table, year, title, universe,
#' and n (where n is the number of variables in the table).
#'
#' @examples
#' acstabs
"acstabs"


#' ACS variables metadata
#'
#' @source Constructed from Census documentation <https://www.census.gov/programs-surveys/acs/technical-documentation/table-shells.html>.
#'
#' @format
#' Tibble with 1 row per ACS variable, with columns table, line, variable, label, release, year, title, universe,
#' uvariable, where uvariable is a uniform variable name (experimental) that puts an underscore in names from early years,
#' which do not have underscores.
#'
#' @examples
#' acsvars
"acsvars"

#' Geometry data for areas of interest
#'
#' @source Constructed using tidycensus
#'
#' @format
#' Tibble with 1 row per area, with affgeoid, geoid, fullname, and geomery.
#'
#' @examples
#' geometry
"geometry"

#' Geographic identification variables for areas of interest
#'
#' @source Constructed using tidycensus
#'
#' @format
#' Tibble with 1 row per area, with affgeoid, geoid, fullname, and many other identification variables.
#'
#' @examples
#' xwalk
"xwalk"


