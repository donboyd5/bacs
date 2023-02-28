

# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))

# constants ---------------------------------------------------------------

# "external" folders
xdacs <- default_xdacs

# folders in this project
dacs <- here::here("data", "acs")



# download files ----------------------------------------------------------

## ONETIME - get the big 5year file of all table-based ACS files -----
url <- "https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/5YRData.zip"

# set timeout
download.file(url, fpath, mode="wb")


## ONETIME - download individual files ----
tabnames <- c("B01001", "B01002", "B01003", "B07001", "B07009", "B07409", "B15003", "B25071")

f <- function(tab, destdir, overwrite=FALSE){
  urlbase <- "https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData"

  str_sub(tab, 1, 1) <- str_to_lower(str_sub(tab, 1, 1))
  fname <- paste0("acsdt5y2021-", tab, ".dat")
  url <- path(urlbase, fname)

  fpath <- path(destdir, fname)
  if(!file_exists(fpath) | overwrite){
    print(paste0("downloading... ", fname))
    download.file(url, fpath, mode="wb")
  }
  return()
}

# purrr::map(tabnames, f, xdacs)

## read from big zip file ----
# 5YRData.zip\data\prt01\prod\sumfile_new\output\2021\5YRData
zpath <- path(xdacs, "5YRData.zip")
fname <- "acsdt5y2021-b01001.dat"
zsub <- path(r"(data\prt01\prod\sumfile_new\output\2021\5YRData)", fname)

a <- proc.time()
df1 <- read_delim(unz(zpath, zsub))
b <- proc.time()
b - a # 3.6 secs

system.time(df1 <- read_delim(unz(zpath, zsub))) # 2.8 secs
system.time(df2 <- read_delim(path(xdacs, fname))) # 1.3 secs



