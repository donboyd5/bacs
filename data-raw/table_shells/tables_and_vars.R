
# get detailed ACS table shells for all years from Census source data ----

# https://www.census.gov/programs-surveys/acs/technical-documentation/table-shells.html

# https://www2.census.gov/programs-surveys/acs/summary_file/2021/sequence-based-SF/documentation/user_tools/ACS2021_Table_Shells.xlsx
# https://www2.census.gov/programs-surveys/acs/summary_file/2020/documentation/user_tools/ACS2020_Table_Shells.xlsx
# https://www2.census.gov/programs-surveys/acs/summary_file/2019/documentation/user_tools/ACS2019_Table_Shells.xlsx
# https://www2.census.gov/programs-surveys/acs/summary_file/2018/documentation/user_tools/ACS2018_Table_Shells.xlsx
# https://www2.census.gov/programs-surveys/acs/summary_file/2017/documentation/user_tools/ACS2017_Table_Shells.xlsx
# https://www2.census.gov/programs-surveys/acs/summary_file/2016/documentation/user_tools/ACS2016_Table_Shells.xlsx
# https://www2.census.gov/programs-surveys/acs/summary_file/2015/documentation/user_tools/ACS2015_Table_Shells.xlsx
# https://www2.census.gov/programs-surveys/acs/summary_file/2014/documentation/user_tools/ACS2014_Table_Shells.xlsx
# https://www2.census.gov/programs-surveys/acs/summary_file/2013/documentation/user_tools/ACS2013_TableShells.xls
# https://www2.census.gov/programs-surveys/acs/summary_file/2012/documentation/5_year/user_tools/ACS2012_5-Year_TableShells.xls
# https://www2.census.gov/programs-surveys/acs/summary_file/2011/documentation/5_year/user_tools/ACS2011_5-Year_TableShells.xls
# https://www2.census.gov/programs-surveys/acs/summary_file/2010/documentation/5_year/user_tools/ACS2010_5-Year_TableShells.xls
# https://www2.census.gov/programs-surveys/acs/summary_file/2009/documentation/5_year/user_tools/ACS2009_5-Year_TableShells.xls


# https://www2.census.gov/programs-surveys/acs/summary_file/2012/documentation/3_year/user_tools/ACS2012_3-Year_TableShells.xls
# https://www2.census.gov/programs-surveys/acs/summary_file/2012/documentation/1_year/user_tools/ACS2012_1-Year_TableShells.xls

# https://www2.census.gov/programs-surveys/acs/summary_file/2011/documentation/3_year/user_tools/ACS2011_3-Year_TableShells.xls
# https://www2.census.gov/programs-surveys/acs/summary_file/2011/documentation/1_year/user_tools/ACS2011_1-Year_TableShells.xls

# https://www2.census.gov/programs-surveys/acs/summary_file/2010/documentation/3_year/user_tools/ACS2010_3-Year_TableShells.xls
# https://www2.census.gov/programs-surveys/acs/summary_file/2010/documentation/1_year/user_tools/ACS2010_1-Year_TableShells.xls

# https://www2.census.gov/programs-surveys/acs/summary_file/2009/documentation/3_year/user_tools/ACS2009_3-Year_TableShells.xls
# https://www2.census.gov/programs-surveys/acs/summary_file/2009/documentation/1_year/user_tools/ACS2009TableShells.xls



# setup -------------------------------------------------------------------
library(devtools)
library(usethis)
source(here::here("data-raw", "libraries.r"))
library(bacs)

xdshells <- r"(E:\data\acs\sf\table_shells)"
# bacs:::default_xdacs


# download all shells -----------------------------------------------------
# https://www2.census.gov/programs-surveys/acs/summary_file/2020/documentation/user_tools/ACS2020_Table_Shells.xlsx

url1 <- "https://www2.census.gov/programs-surveys/acs/summary_file/"
url2 <- "/documentation/user_tools/"
(urlbase20142020 <- paste0(url1, 2014:2020, url2))
(fn20142020 <- paste0("ACS", 2014:2020, "_Table_Shells.xlsx"))
(urls20142020 <- paste0(urlbase20142020, fn20142020))

urls <- c("https://www2.census.gov/programs-surveys/acs/summary_file/2021/sequence-based-SF/documentation/user_tools/ACS2021_Table_Shells.xlsx",
          urls20142020,
          "https://www2.census.gov/programs-surveys/acs/summary_file/2013/documentation/user_tools/ACS2013_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2012/documentation/5_year/user_tools/ACS2012_5-Year_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2011/documentation/5_year/user_tools/ACS2011_5-Year_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2010/documentation/5_year/user_tools/ACS2010_5-Year_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2009/documentation/5_year/user_tools/ACS2009_5-Year_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2012/documentation/3_year/user_tools/ACS2012_3-Year_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2012/documentation/1_year/user_tools/ACS2012_1-Year_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2011/documentation/3_year/user_tools/ACS2011_3-Year_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2011/documentation/1_year/user_tools/ACS2011_1-Year_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2010/documentation/3_year/user_tools/ACS2010_3-Year_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2010/documentation/1_year/user_tools/ACS2010_1-Year_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2009/documentation/3_year/user_tools/ACS2009_3-Year_TableShells.xls",
          "https://www2.census.gov/programs-surveys/acs/summary_file/2009/documentation/1_year/user_tools/ACS2009TableShells.xls"
          )

# urls5 <- urls[str_detect(urls, "3_year", negate = TRUE)]; (urls5 <- urls5[str_detect(urls5, "1_year", negate = TRUE)])
# path_file(urls5)

urls
for(url in urls){
  print(url)
  fname <- path_file(url)
  print(fname)
  dest <- path(xdshells, fname)
  print(dest)
  download.file(url, dest, mode="wb")
}


# create a unified set of table shells ------------------------------------
# variables we want to have for all shells
# table, line, variable, label, title, universe, release, uvariable
# where:
#   variable is the variable name given in the shell
#   uvariable is a uniform variable name that follows current rules with underscore
#   release tells which release(s) the shell applies to

# this section is based on the exploration in the next section ----
# get 5-year ACS file paths
urls5 <- urls[str_detect(urls, "3_year", negate = TRUE)]; (urls5 <- urls5[str_detect(urls5, "1_year", negate = TRUE)])
(fpaths <- path(xdshells, path_file(urls5)))
# use the manually created xlsx file for 2013
fpaths <- ifelse(fpaths=="E:/data/acs/sf/table_shells/ACS2013_TableShells.xls",
                 "E:/data/acs/sf/table_shells/ACS2013_TableShells.xlsx",
                 fpaths)
fpaths

pathy <- function(fpath){
  # get year from the file path
  str_sub(path_file(fpath), 4, 7)
}
# pathy(fpaths) |> sort()

fget <- function(fpath, year, sheet){
  year <- pathy(fpath)
  print(year)

  if(year %in% c(2009:2012, 2015:2021)) {
    sheet=1
  } else if(year==2013) {
    sheet="Sheet2"
  } else if(year==2014) {
    sheet="Sheet4"
  } else sheet="ERROR"

  df1 <- read_excel(fpath, sheet=sheet)

  if(year %in% 2009:2012){
    df2 <- df1 |>
      select(table=1, line=2, variable=3, label=4) |>
      mutate(release="5")
  } else{
    df2 <- df1 |>
      select(table=1, line=2, variable=3, label=4, release=5)
  }

  df3 <- df2 |>
    mutate(line=as.integer(line),
           year=as.integer(year)) |>
    group_by(table) |>
    mutate(title=label[1], universe=label[2], release=release[1]) |>
    filter(line >=1) |>
    filter(!is.na(variable)) |>
    ungroup() |>
    mutate(universe=str_remove(universe, "Universe:  "),
           uvariable=case_when(
             year %in%  2009:2014 ~
               paste0(str_sub(variable, 1, -4), "_", str_sub(variable, -3, -1)),
             TRUE ~ variable))
  return(df3)
}

# purrr::walk(fpaths, fget)
df <- purrr::map_dfr(fpaths, fget)
skim(df)
tmp <- count(df, table)
tmp <- count(df, variable, uvariable)
tmp <- count(df, universe)
tmp <- count(df, universe, year)

acsvars <- df
# acsvars <- bacs::acsshells
usethis::use_data(acsvars, overwrite = TRUE)


# create acstabs long ----
acstabs <- acsvars |>
  summarise(n=n(), .by=c(table, year, title, universe))
usethis::use_data(acstabs, overwrite = TRUE)



# BELOW HERE: explore structure of the files ----

(fpaths <- path(xdshells, path_file(urls)))

## 2019-2012 ----
# table, line, variable without _, label (diff names)
# get title from label first row, universe from label 2nd row

fpath <- "E:/data/acs/sf/table_shells/ACS2009_5-Year_TableShells.xls"
fpath <- "E:/data/acs/sf/table_shells/ACS2010_5-Year_TableShells.xls"
fpath <- "E:/data/acs/sf/table_shells/ACS2011_5-Year_TableShells.xls"
fpath <- "E:/data/acs/sf/table_shells/ACS2012_5-Year_TableShells.xls"
(df1 <- read_excel(fpath))
df2 <- df1 |>
  select(table=1, line=2, variable=3, label=4) |>
  group_by(table) |>
  mutate(title=label[1], universe=label[2]) |>
  filter(line >=1) |>
  ungroup() |>
  mutate(universe=str_remove(universe, "Universe:  "),
         uvariable=paste0(str_sub(variable, 1, -4), "_", str_sub(variable, -3, -1)))
df2

fpaths
## 2013-2014 ----
# fpath <- "E:/data/acs/sf/table_shells/ACS2013_TableShells.xls" # original, bad format
fpath <- "E:/data/acs/sf/table_shells/ACS2013_TableShells.xlsx" # manually saved as xlsx; use Sheet2
fpath <- "E:/data/acs/sf/table_shells/ACS2014_Table_Shells.xlsx" # Sheet4
(df1 <- read_excel(fpath, sheet="Sheet4"))
df2 <- df1 |>
  select(table=1, line=2, variable=3, label=4, release=5) |>
  group_by(table) |>
  mutate(title=label[1], universe=label[2], release=release[1]) |>
  filter(line >=1) |>
  ungroup() |>
  mutate(universe=str_remove(universe, "Universe:  "),
         uvariable=paste0(str_sub(variable, 1, -4), "_", str_sub(variable, -3, -1)))
df2

# 2015-2021  variable name has _, sheet is #1
year <- 2021
(fpath <- fpaths[str_detect(fpaths, as.character(year))])
(df1 <- read_excel(fpath, sheet=1))
df2 <- df1 |>
  select(table=1, line=2, variable=3, label=4, release=5) |>
  group_by(table) |>
  mutate(title=label[1], universe=label[2], release=release[1]) |>
  filter(line >=1) |>
  ungroup() |>
  mutate(universe=str_remove(universe, "Universe:  "),
         uvariable=variable)
df2


# E:/data/acs/sf/table_shells/ACS2014_Table_Shells.xlsx
# E:/data/acs/sf/table_shells/ACS2014_TableShells.xlsx
