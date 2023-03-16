
#' Read a single ACS table and return the full table for geographies of interest
#'
#' @param tabname Census Bureau name for the table (e.g., B01001) as a string.
#' @param year 4-digit numeric value, must be a 5-year ACS endyear, default 2021
#'
#' @returns Tibble with the full data for the ACS table.
#' @export
#'
#' @examples
#' df <- get_acstab("B01003")
get_acstab <- function(tabname, year=2021, geometry=TRUE) {

  if(year <= 2014 & geometry==TRUE) {
    msg <- paste0("Resetting geometry to FALSE for years <= 2014. Year: ", year)
    print(msg)
    geometry <- FALSE
  }

  us1 <- tidycensus::get_acs(geography = "us",
                 table = tabname, year=year, survey="acs5",
                 state=NULL,
                 geometry = geometry, keep_geo_vars = geometry,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                 cache_table = TRUE) |> # can't download geometry for 2011??
    dplyr::mutate(AFFGEOID="0100000US", geotype="nation")
  # either NAME.x or NAME.y will serve as fullname

  states1 <- tidycensus::get_acs(geography = "state",
                     table = tabname, year=year, survey="acs5",
                     state=NULL,
                     geometry = geometry, keep_geo_vars = geometry,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                     cache_table = TRUE) |>
    dplyr::mutate(AFFGEOID=paste0("0400000US", GEOID), geotype="state")
  # either NAME.x or NAME.y will serve as fullname

  nycounty1 <- tidycensus::get_acs(geography = "county",
                       table = tabname, year=year, survey="acs5",
                       state="NY",
                       geometry = geometry, keep_geo_vars = geometry,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                       cache_table = TRUE) |>
    dplyr::mutate(AFFGEOID=paste0("0500000US", GEOID), geotype="county")
  # NAME.y is fullname

  nycousub1 <- tidycensus::get_acs(geography = "county subdivision",
                       table = tabname, year=year, survey="acs5",
                       state="NY",
                       geometry = geometry, keep_geo_vars = geometry,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                       cache_table = TRUE) |>
    # tidycensus returns some NA AFFGEOIDs so make sure we have them
    dplyr::mutate(AFFGEOID=paste0("0600000US", GEOID), geotype="cousub")
    # NAME.y is fullname!

  nyplace1 <- tidycensus::get_acs(geography = "place",
                      table = tabname, year=year, survey="acs5",
                      state="NY",
                      geometry = geometry, keep_geo_vars = geometry,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                      cache_table = TRUE) |>
    dplyr::mutate(AFFGEOID=paste0("1600000US", GEOID), geotype="place")
  # NAME.y is fullname

  nyschdistelem1 <- tidycensus::get_acs(geography = "school district (elementary)",
                            table = tabname, year=year, survey="acs5",
                            state="NY",
                            geometry = geometry, keep_geo_vars = geometry,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                            cache_table = TRUE) |>
    dplyr::mutate(AFFGEOID=paste0("9500000US", GEOID), geotype="schdist_elem")

  nyschdistsecond1 <- tidycensus::get_acs(geography = "school district (secondary)",
                              table = tabname, year=year, survey="acs5",
                              state="NY",
                              geometry = geometry, keep_geo_vars = geometry,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                              cache_table = TRUE) |>
    dplyr::mutate(AFFGEOID=paste0("9600000US", GEOID), geotype="schdist_second")

  nyschdistunified1 <- tidycensus::get_acs(geography = "school district (unified)",
                               table = tabname, year=year, survey="acs5",
                               state="NY",
                               geometry = geometry, keep_geo_vars = geometry,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                               cache_table = TRUE) |>
    dplyr::mutate(AFFGEOID=paste0("9700000US", GEOID), geotype="schdist_unified")

  stack1 <- dplyr::bind_rows(us1, states1, nycounty1, nycousub1, nyplace1,
                      nyschdistelem1, nyschdistsecond1, nyschdistunified1) |>
    tibble::as_tibble()

  if("NAME" %in% colnames(stack1)) {
    stack2 <- stack1 |>
      rename(fullname=NAME)
  } else {
    stack2 <- stack1 |>
      mutate(fullname=NA_character_)
  } # has fullname, no longer has NAME

  if("NAME.y" %in% colnames(stack2)) {
    stack3 <- stack2 |>
      mutate(fullname=ifelse(is.na(fullname),
                             NAME.y,
                             fullname)) |>
      select(-NAME.x, -NAME.y)
  } else stack3 <- stack2

  stack4 <- stack3 |>
    btools::lcnames() |>
    dplyr::mutate(year=!!year, dataset="acs5") |>
    dplyr::relocate(affgeoid, geoid, fullname)

  stack4
}



#' Enhance a single ACS table and return the full table for geographies of interest
#'
#' @param tabdf Census Bureau name for the table (e.g., B01001) as a string.
#' @param year 4-digit numeric value, must be a 5-year ACS endyear, default 2021
#'
#' @returns Tibble with the full data for the ACS table.
#' @export
#'
#' @examples
#' df1 <- get_acstab("B01001")
#' df2 <- enhance(df1)
enhance <- function(tabdf, year=2021){
  tabdf2 <- tabdf |>
    dplyr::select(-name, -dataset) |>
    dplyr::left_join(bacs::xwalkny |>
                dplyr::select(affgeoid, fullname, shortname, shortestname, geotype, nygeotype,
                       stabbr, county, countyname),
              by = join_by(affgeoid)) |>
    dplyr::select(affgeoid, geoid, fullname, shortname, shortestname, geotype, nygeotype,
           stabbr, county, countyname, everything()) |>
    # tidycensus apparently has already made variable names uniform so use uvariable (my uniform variable)
    #   rather than variable as given in census documentation
    dplyr::left_join(bacs::acsvars |> select(variable=uvariable, line, label, year, title, universe),
              by = join_by(variable, year)) |>
    dplyr::relocate(line, label, .after=variable)
  tabdf2
}


#' Get and enhance a single ACS table and return the full table for geographies of interest
#'
#' @param tabname Census Bureau name for the table (e.g., B01001) as a string.
#' @param year 4-digit numeric value, must be a 5-year ACS endyear, default 2021
#'
#' @returns Tibble with the full data for the ACS table.
#' @export
#'
#' @examples
#' df2 <- get_enhanced("B01003")
get_enhanced <- function(tabname, year=2021){
  tabdf <- get_acstab(tabname, year=year)
  enhance(tabdf)
}


#' Get number of ACS variables for all tables, selected years
#'
#' @param years
#'
#' @returns Tibble with tables as rows, years as columns, # of variables as cells.
#' @export
#'
#' @examples
#' acstabs_wide(c(2011, 2016, 2021))
acstabs_wide <- function(years){
  bacs::acstabs |>
    dplyr::filter(year %in% years) |>
    dplyr::select(table, title, universe, year, nvars) |>
    dplyr::arrange(year) |>
    tidyr::pivot_wider(names_from = year, values_from = nvars)
}


#' Find ACS tables containing a given term
#'
#' @param searchterm String to search for in the `title` field of `tabs`.
#' @param tabs Tibble that contains field `title`.
#'
#' @returns Tibble with the resulting table information.
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









