

# https://r-pkgs.org/data.html

## read from big zip file ----
# 5YRData.zip\data\prt01\prod\sumfile_new\output\2021\5YRData
# zpath <- path(xdacs, "5YRData.zip")
# fname <- "acsdt5y2021-b01001.dat"
# zsub <- path(r"(data\prt01\prod\sumfile_new\output\2021\5YRData)", fname)
#
# a <- proc.time()
# df1 <- read_delim(unz(zpath, zsub))
# b <- proc.time()
# b - a # 3.6 secs
#
# system.time(df1 <- read_delim(unz(zpath, zsub))) # 2.8 secs
# system.time(df2 <- read_delim(path(xdacs, fname))) # 1.3 secs


#' Read a single ACS table and return the full table
#'
#' @param tabname Census Bureau name for the table (e.g., B01001) as a string.
#' @param xdacs Local folder where table is saved (or default).
#'
#' @return Tibble with the full data for the ACS table.
#' @export
#'
#' @examples
#' df <- read_acstab("B01003")
read_acstab <- function(tabname, xdacs=bacs:::default_xdacs) {
  # load("R/sysdata.rda")
  stringr::str_sub(tabname, 1, 1) <- stringr::str_to_lower(stringr::str_sub(tabname, 1, 1))
  fname <- paste0("acsdt5y2021-", tabname, ".dat")

  zpath <- fs::path(xdacs, "5YRData.zip")
  zsub <- fs::path(r"(data\prt01\prod\sumfile_new\output\2021\5YRData)", fname)

  read_delim(unz(zpath, zsub))
}


#' Read, enhance, and save an ACS table
#'
# Read a raw ACS table from disk, filter it, enhance it with geographic
# identification information, and save it as an rds file.
#'
#' @param tabname Census Bureau name for the table (e.g., B01001) as a string.
#' @param xdacs Local folder where table is saved (or default).
#'
#' @return NULL
#' @export
#'
#' @examples
#' save_tabny("B01003", overwrite=TRUE)
save_tabny <- function(tabname, xdacs=bacs:::default_xdacs, overwrite=FALSE) {
  stringr::str_sub(tabname, 1, 1) <- stringr::str_to_lower(stringr::str_sub(tabname, 1, 1))
  fname <- paste0(tabname, ".rds")
  fpath <- fs::path(xdacs, fname)

  if(fs::file_exists(fpath) & !overwrite){
    print(paste0("No: ", fname, " already exists and overwrite=FALSE."))
    return(NULL)
  }
  print(paste0("Getting and saving... ", fname))

  df <- read_acstab(tabname, xdacs) |>
    filter(GEO_ID %in% bacs:::default_geoids) |>
    left_join(bacs::xwalkny |>
              select(STUSAB, GEO_ID, COUNTY, geotype, nygeotype, NAME,
                     shortname, countyname),
              by = join_by(GEO_ID)) |>
    relocate(STUSAB, COUNTY, geotype, nygeotype, NAME,
             shortname, countyname, .after=GEO_ID)

  saveRDS(df, fpath)
  return(NULL)
}


# tabnames <- c("B01001", "B01002", "B01003", "B07001", "B07009", "B07409", "B15003", "B25071")
#
# save_tabny("B01003", overwrite=TRUE)
#
# purrr::map(tabnames, save_tabny, overwrite=TRUE)


#' Read a previously saved enhanced ACS table from disk
#'
#' Read an ACS table that has been enhanced with additional geographic
#' identification information.
#'
#' @param tabname Census Bureau name for the table (e.g., B01001) as a string.
#' @param xdacs Local folder where table is saved (or default).
#'
#' @return Tibble with the enhanced filtered ACS table.
#' @export
#'
#' @examples
#' read_tabny("B01001")
read_tabny <- function(tabname, xdacs=bacs:::default_xdacs) {
  stringr::str_sub(tabname, 1, 1) <- stringr::str_to_lower(stringr::str_sub(tabname, 1, 1))
  fname <- paste0(tabname, ".rds")
  fpath <- fs::path(xdacs, fname)
  readRDS(fpath)
}

# read_tabny(tabnames[2])


