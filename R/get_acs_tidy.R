
#' Read a single ACS table and return the full table for geographies of interest
#'
#' @param tabname Census Bureau name for the table (e.g., B01001) as a string.
#'
#' @return Tibble with the full data for the ACS table.
#' @export
#'
#' @examples
#' df <- get_acstab("B01003")
get_acstab <- function(tabname, year=2021) {

  us1 <- tidycensus::get_acs(geography = "us",
                 table = tabname, year=year, survey="acs5",
                 state=NULL,
                 cache_table = TRUE) |>
    dplyr::mutate(affgeoid="0100000US") # paste0("0100000US", GEOID)

  states1 <- tidycensus::get_acs(geography = "state",
                     table = tabname, year=year, survey="acs5",
                     state=NULL,
                     cache_table = TRUE) |>
    dplyr::mutate(affgeoid=paste0("0400000US", GEOID))

  nycounty1 <- tidycensus::get_acs(geography = "county",
                       table = tabname, year=year, survey="acs5",
                       state="NY",
                       cache_table = TRUE) |>
    dplyr::mutate(affgeoid=paste0("0500000US", GEOID))

  nycousub1 <- tidycensus::get_acs(geography = "county subdivision",
                       table = tabname, year=year, survey="acs5",
                       state="NY",
                       cache_table = TRUE) |>
    dplyr::mutate(affgeoid=paste0("0600000US", GEOID))

  nyplace1 <- tidycensus::get_acs(geography = "place",
                      table = tabname, year=year, survey="acs5",
                      state="NY",
                      cache_table = TRUE) |>
    dplyr::mutate(affgeoid=paste0("1600000US", GEOID))

  nyschdistelem1 <- tidycensus::get_acs(geography = "school district (elementary)",
                            table = tabname, year=year, survey="acs5",
                            state="NY",
                            cache_table = TRUE) |>
    dplyr::mutate(affgeoid=paste0("9500000US", GEOID))

  nyschdistsecond1 <- tidycensus::get_acs(geography = "school district (secondary)",
                              table = tabname, year=year, survey="acs5",
                              state="NY",
                              cache_table = TRUE) |>
    dplyr::mutate(affgeoid=paste0("9600000US", GEOID))

  nyschdistunified1 <- tidycensus::get_acs(geography = "school district (unified)",
                               table = tabname, year=year, survey="acs5",
                               state="NY",
                               cache_table = TRUE) |>
    dplyr::mutate(affgeoid=paste0("9700000US", GEOID))

  stack1 <- dplyr::bind_rows(us1, states1, nycounty1, nycousub1, nyplace1,
                      nyschdistelem1, nyschdistsecond1, nyschdistunified1) |>
    tibble::as_tibble() |>
    btools::lcnames() |>
    dplyr::mutate(year=!!year, dataset="acs5") |>
    dplyr::relocate(affgeoid)

  stack1
}

# df <- get_acstab("B01001")
# glimpse(df)
# skim(df)
#
# xwalkny


#' Enhance a single ACS table and return the full table for geographies of interest
#'
#' @param df Census Bureau name for the table (e.g., B01001) as a string.
#'
#' @return Tibble with the full data for the ACS table.
#' @export
#'
#' @examples
#' df1 <- get_acstab("B01001")
#' df2 <- enhance(df1)
enhance <- function(df){
  df2 <- df |>
    dplyr::select(-name, -dataset) |>
    dplyr::left_join(bacs::xwalkny |>
                dplyr::select(affgeoid, fullname, shortname, shortestname, geotype, nygeotype,
                       stabbr, county, countyname),
              by = join_by(affgeoid)) |>
    dplyr::select(affgeoid, geoid, fullname, shortname, shortestname, geotype, nygeotype,
           stabbr, county, countyname, everything()) |>
    # left_join(bacs::acsvarstc |> select(variable, line, label, year),
    dplyr::left_join(bacs::acsvars |> select(variable, line, label, year, title, universe),
              by = join_by(variable, year)) |>
    dplyr::relocate(line, label, .after=variable)
  df2
}
# df2 <- enhance(df)


#' Get and enhance a single ACS table and return the full table for geographies of interest
#'
#' @param tabname Census Bureau name for the table (e.g., B01001) as a string.
#'
#' @return Tibble with the full data for the ACS table.
#' @export
#'
#' @examples
#' df2 <- get_enhanced("B01003")
get_enhanced <- function(tabname){
  df <- get_acstab(tabname)
  enhance(df)
}


#' Get number of ACS variables for all tables, selected years
#'
#' @param years
#'
#' @return Tibble with tables as rows, years as columns, # of variables as cells.
#' @export
#'
#' @examples
#' acstabs_wide(c(2011, 2016, 2021))
acstabs_wide <- function(years){
  bacs::acstabs |>
    dplyr::filter(year %in% years) |>
    dplyr::select(table, title, universe, year, n) |>
    dplyr::arrange(year) |>
    tidyr::pivot_wider(names_from = year, values_from = n)
}


#' Find ACS tables containing a given term
#'
#' @param searchterm String to search for in the `title` field of `tabs`.
#' @param tabs Tibble that contains field `title`.
#'
#' @return Tibble with the resulting table information.
#' @export
#'
#' @examples
#' find_acstab("age")
#' find_acstab("age", acstabs_wide(c(2011, 2016, 2021)))
find_acstab <- function(searchterm, tabs=bacs::acstabs){
  tabs |>
    dplyr::filter(stringr::str_detect(stringr::str_to_upper(title),
                                      stringr::str_to_upper(searchterm)))
}









