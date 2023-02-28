# goals ----
#   save two files in the data directory
#     geometry.rda
#     xwalkny_tidy.rda, for eventual merging with xwalkny_newformat
#       to create xwalkny.rda


# information ----

## geoid elements ----


# 1-3 sumlevel 070
# 4-7 ?? all zeros 0000
# 8-9 US US
# 10-11 stfips 36
# 12-14 county 115
# 15-19 cousub 02561
# 20-24 place 02550

## selected summary levels of interest ----
# https://api.census.gov/data/2021/acs/acs5/geography.html
# 010	us
# 020	region
# 030	division
# 040	state
# 050	state› county
# 060	state› county› county subdivision
# 067	state› county› county subdivision› subminor civil division
# 070	state› county› county subdivision› place/remainder (or part)
# 140	state› county› tract
# 150	state› county› tract› block group
# 155	state› place› county (or part)
# 160	state› place
# 170	state› consolidated city
# 400	urban area
# 410	urban area› state (or part)
# 430	urban area› state (or part)› county (or part)

# 950	state› school district (elementary)
# 960	state› school district (secondary)
# 970	state› school district (unified)


# Area Type	GEOID Structure	Number of Digits	Example Geographic Area	Example GEOID
# State	STATE	2	Texas	48
# County	STATE+COUNTY	2+3=5	Harris County, TX	48201
# County Subdivision	STATE+COUNTY+COUSUB	2+3+5=10	Pasadena CCD, Harris County, TX	4820192975
# Places	STATE+PLACE	2+5=7	Houston, TX	4835000
# Census Tract	STATE+COUNTY+TRACT	2+3+6=11	Census Tract 2231 in Harris County, TX	48201223100
# Block Group	STATE+COUNTY+TRACT+BLOCK GROUP	2+3+6+1=12	Block Group 1 in Census Tract 2231 in Harris County, TX	482012231001
# Block*	STATE+COUNTY+TRACT+BLOCK	2+3+6+4=15 (Note – some blocks also contain a one character suffix (A, B, C, ect.)	Block 1050 in Census Tract 2231 in Harris County, TX	482012231001050
# Congressional District (113th Congress)	STATE+CD	2+2=4	Connecticut District 2	902
# State Legislative District (Upper Chamber)	STATE+SLDU	2+3=5	Connecticut State Senate District 33	9033
# State Legislative District (Lower Chamber)	STATE+SLDL	2+3=5	Connecticut State House District 147	9147
# ZCTA **	ZCTA	5	Suitland, MD ZCTA	20746
#


# setup -------------------------------------------------------------------

library(devtools)
library(usethis)
source(here::here("data-raw", "libraries.r"))
# load(here::here("R", "sysdata.rda"))


# constants ---------------------------------------------------------------

# xdacs <- default_xdacs # external directory for acs


# data ---------------------------------------------------------
# allow comparison to geography in the new-format acs
xwcomp <- xwalkny_newformat


data(acs5_geography)
count(acs5_geography |> filter(year==2021), geography)
# geography       n
# <chr>       <int>
#   1 block group   370
# 2 county         66
# 3 state          39
# 4 tract         551
# 5 us             53

# decomposing AFFGEOID


# when we don't request geometry, we get GEOID, NAME, variable, estimate, moe

# geography: https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus

# get acs data for relevant geographies ----
us1 <- get_acs(geography = "us",
               table = "B01003", year=2021, survey="acs5",
               state=NULL,
               geometry = TRUE, keep_geo_vars = TRUE,  # keep_geo_vars TRUE only works if we set geometry=TRUE
               cache_table = TRUE, show_call = TRUE)
glimpse(us1) # 1 record, has NAME.x and NAME.y; note that variable is B01003_001, with estimate and moe as columns
# AFFGEOID is the long geo identifier  "0100000US"; GEOID is short;
# example:
#   AFFGEOID <chr> "0100000US", GEOID    <chr> "1"
#   NAME.x   <chr> "United States"
#   NAME.y   <chr> "United States"
# NAME not on file
us1 |> as_tibble() |> select(-geometry) |> filter(AFFGEOID=="0100000US") |> select(AFFGEOID, GEOID, NAME.x, NAME.y, variable, estimate)
xwcomp |> filter(GEO_ID=="0100000US") |> select(STUSAB, GEO_ID, NAME, pop20215)


# urban areas do not seem to work with tidycensus
# urban1 <- get_acs(geography = "urban area",
#                table = "B01003", year=2021, survey="acs5",
#                state=NULL,
#                geometry = TRUE, keep_geo_vars = TRUE,  # keep_geo_vars TRUE only works if we set geometry=TRUE
#                cache_table = TRUE, show_call = TRUE)


states1 <- get_acs(geography = "state",
               table = "B01003", year=2021, survey="acs5",
               state=NULL,
               geometry = TRUE, keep_geo_vars = TRUE,  # keep_geo_vars TRUE only works if we set geometry=TRUE
               cache_table = TRUE, show_call = TRUE)
glimpse(states1) # 52: 50 states, DC, and PR; STUSPS is state abbreviation (not STUSAB)
# has NAME.x, NAME.y, LSAD, ALAND, AWATER,
# STATEFP, STATENS (?),
# example:
#   AFFGEOID <chr> "0400000US56", GEOID    <chr> "56", STUSPS   <chr> "WY",
#   NAME.x   <chr> "Wyoming"
#   NAME.y   <chr> "Wyoming"
# NAME not on file
#   STATEFP  <chr> "56", STATENS  <chr> "01779807"
states1 |> as_tibble() |> select(-geometry) |> filter(AFFGEOID=="0400000US56") |> select(AFFGEOID, GEOID, NAME.x, NAME.y, estimate)
xwcomp |> filter(GEO_ID=="0400000US56") |> select(STUSAB, GEO_ID, NAME, pop20215)

nycounty1 <- get_acs(geography = "county",
                     table = "B01003", year=2021, survey="acs5",
                     state="NY",
                     geometry = TRUE, keep_geo_vars = TRUE,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                     cache_table = TRUE, show_call = TRUE)
glimpse(nycounty1) # 62 recs: 57 + 5 boroughs
# ns(nycounty1)
# STATEFP, COUNTYFP, COUNTYNS(?), STATE_NAME
# has
# example
#   AFFGEOID: "0500000US36017", GEOID: "36017"
#   NAME.x     <chr> "Chenango"
#   NAMELSAD   <chr> "Chenango County"
#   NAME.y     <chr> "Chenango County, New York" -- this is what we get when no geometry; it matches NAME in newformat
# NAME NOT on file

nycounty1 |> as_tibble() |> select(-geometry) |> filter(AFFGEOID=="0500000US36017") |> select(AFFGEOID, GEOID, NAME.x, NAME.y, estimate)
xwcomp |> filter(GEO_ID=="0500000US36017") |> select(STUSAB, GEO_ID, NAME, pop20215)


nycousub1 <- get_acs(geography = "county subdivision",
                     table = "B01003", year=2021, survey="acs5",
                     state="NY",
                     geometry = TRUE, keep_geo_vars = TRUE,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                     cache_table = TRUE, show_call = TRUE)
glimpse(nycousub1) # 1,023 recs
# ns(nycousub1)
# example:
#    AFFGEOID   <chr> "0600000US3606144919", GEOID      <chr> "3606144919"
#    NAME.x     <chr> "Manhattan"
#    NAME.y     <chr> "Manhattan borough, New York County, New York"
#    NAMELSAD   <chr> "Manhattan borough" <-- notice
#    COUNTYFP   <chr> "061",
#    COUSUBFP   <chr> "44919", COUSUBNS   <chr> "00979190"
#    NAMELSADCO <chr> "New York County", LSAD       <chr> "21" (??)
# NAME NOT on file

# note that when we don't get geometry, we get:
#   geoid but not affgeoid, and
#   NAME is NAME.y (full name), not NAME.x (short name)
#   example:
#  nycousub1 |> filter(GEOID=="3606144919")
#    3606144919 Manhattan borough, New York County, New York


nycousub1 |> as_tibble() |> select(-geometry) |> filter(AFFGEOID=="0600000US3606144919") |> select(AFFGEOID, GEOID, NAME.x, NAME.y, estimate)
xwcomp |> filter(GEO_ID=="0600000US3606144919") |> select(STUSAB, GEO_ID, NAME, pop20215)
# good; NAME.y in tidycensus matches NAME in newformat


nyplace1 <- get_acs(geography = "place",
                     table = "B01003", year=2021, survey="acs5",
                     state="NY",
                     geometry = TRUE, keep_geo_vars = TRUE,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                     cache_table = TRUE, show_call = TRUE)
glimpse(nyplace1) # 1,293 recs; note that they do not have county identification
# ns(nyplace1)
# now we have PLACEFP, PLACENS,
# example:
#   AFFGEOID   <chr> "1600000US3655816", GEOID      <chr> "3655816",
#   NAME.x     <chr> "Ovid"
#   NAME.y     <chr> "Ovid village, New York"
# NAME NOT on file

nyplace1 |> as_tibble() |> select(-geometry) |> filter(AFFGEOID=="1600000US3655816") |> select(AFFGEOID, GEOID, NAME.x, NAME.y, estimate)
xwcomp |> filter(GEO_ID=="1600000US3655816") |> select(STUSAB, GEO_ID, NAME, pop20215)


nyschdistelem1 <- get_acs(geography = "school district (elementary)",
                    table = "B01003", year=2021, survey="acs5",
                    state="NY",
                    geometry = TRUE, keep_geo_vars = TRUE,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                    cache_table = TRUE, show_call = TRUE)
glimpse(nyschdistelem1) # 14 recs
# ns(nyschdistelem1)

# example:
#   AFFGEOID   <chr> "9500000US3629460", GEOID      <chr> "3629460",
#   ELSDLEA    <chr> "29460"
#   NAME.x     <chr> "Valley Stream Union Free School District 24"
#   NAME.y     <chr> "Valley Stream Union Free School District 24, New York"
# NAME NOT on file
#   does not have other names for the district, does not have county info

nyschdistelem1 |> as_tibble() |> select(-geometry) |> filter(AFFGEOID=="9500000US3629460") |> select(AFFGEOID, GEOID, NAME.x, NAME.y, estimate)
xwcomp |> filter(GEO_ID=="9500000US3629460") |> select(STUSAB, GEO_ID, NAME, pop20215)


nyschdistsecond1 <- get_acs(geography = "school district (secondary)",
                          table = "B01003", year=2021, survey="acs5",
                          state="NY",
                          geometry = TRUE, keep_geo_vars = TRUE,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                          cache_table = TRUE, show_call = TRUE)
glimpse(nyschdistsecond1) # 4 recs
# ns(nyschdistsecond1)

# example:
#   AFFGEOID   <chr> "9600000US3619020", GEOID      <chr> "3619020"
#   SCSDLEA    <chr> "19020",
#   NAME.x     <chr> "Bellmore-Merrick Central High School District"
#   NAME.y     <chr> "Bellmore-Merrick Central High School District, New York"
# NAME NOT on file

nyschdistsecond1 |> as_tibble() |> select(-geometry) |> filter(AFFGEOID=="9600000US3619020") |> select(AFFGEOID, GEOID, NAME.x, NAME.y, estimate)
xwcomp |> filter(GEO_ID=="9600000US3619020") |> select(STUSAB, GEO_ID, NAME, pop20215)


nyschdistunified1 <- get_acs(geography = "school district (unified)",
                            table = "B01003", year=2021, survey="acs5",
                            state="NY",
                            geometry = TRUE, keep_geo_vars = TRUE,  # keep_geo_vars TRUE only works if we set geometry=TRUE
                            cache_table = TRUE, show_call = TRUE)
glimpse(nyschdistunified1) # 667 recs
ns(nyschdistunified1)

# example:
#   AFFGEOID   <chr> "9700000US3616290", GEOID      <chr> "3616290"
#   UNSDLEA    <chr> "16290"
#   NAME.x     <chr> "Kingston City School District"
#   NAME.y     <chr> "Kingston City School District, New York"
# NAME NOT on file

nyschdistunified1 |> as_tibble() |> select(-geometry) |> filter(AFFGEOID=="9700000US3616290") |> select(AFFGEOID, GEOID, NAME.x, NAME.y, estimate)
xwcomp |> filter(GEO_ID=="9700000US3616290") |> select(STUSAB, GEO_ID, NAME, pop20215)



# stack the individual tidycensus geography files -------------------------

# tot recs in tidycensus approach
#  us: 1, states: 52, county: 62
#  cousub: 1,023
#  place: 1,293
#  sds 14, 4, 667
stack1 <- bind_rows(us1, states1, nycounty1, nycousub1, nyplace1,
                 nyschdistelem1, nyschdistsecond1, nyschdistunified1) |>
  as_tibble() |>
  lcnames() |>
  select(-variable, -namelsad) |>
  rename(stabbr=stusps, fullname=name.y, shortname=name.x, pop20215=estimate, popmoe=moe) |>
  relocate(pop20215, popmoe, .before=geometry) |>
  relocate(aland, awater, .before=pop20215) |>
  relocate(state_name, .after=stabbr) |>
  mutate(intidy=TRUE)
glimpse(stack1) # 3,116 rows
ns(stack1)
glimpse(xwcomp) # 3,120 rows -- 2 more US rows + 2 more NY rows -- urban, rural for each

tmp <- stack1 |> select(affgeoid, geoid, name.x, name.y, namelsad)


# save geometry for all geographies ---------------------------------------
geometry1 <- stack1 |>
  select(affgeoid, geoid, fullname, geometry)

geometry <- geometry1
glimpse(geometry)
usethis::use_data(geometry, overwrite = TRUE)

# save xwalkny_tidy for all geographies ---------------------------------------
xwalkny_tidy <- stack1 |>
  select(-geometry)
glimpse(xwalkny_tidy)
usethis::use_data(xwalkny_tidy, overwrite = TRUE)


# REFERENCE information below hwere ----

# get_acs(
#   geography,
#   variables = NULL,
#   table = NULL,
#   cache_table = FALSE,
#   year = 2021,
#   output = "tidy",
#   state = NULL,
#   county = NULL,
#   zcta = NULL,
#   geometry = FALSE,
#   keep_geo_vars = FALSE,
#   shift_geo = FALSE,
#   summary_var = NULL,
#   key = NULL,
#   moe_level = 90,
#   survey = "acs5",
#   show_call = FALSE,
#   ...
# )

get_acstab <- function(acstab, year=2021, geometry=FALSE){
  # get a single table for a single year, for
  # acstab <- "B01003"
  # year <- 2021
  # geometry <- FALSE
  # geometry <- TRUE

  # unfortunately, when geometry=TRUE
  #  get_acs returns two NAME variables, NAME.x and NAME.y
  #  except for school districts
  # otherwise it only returns NAME
  # so drop NAME.y if it exists, and rename NAME.x if it exits
  # do this before bind rows

  nation <- get_acs(geography = "us",
                    table = acstab,
                    year=year,
                    survey="acs5",
                    state=NULL,
                    geometry = geometry,
                    keep_geo_vars = geometry,  # TRUE only works if we set geometry=TRUE
                    cache_table = TRUE) |>
    mutate(geotype="nation") |>
    rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))

  states <- get_acs(geography="state",
                    state=state.abb,
                    geometry = geometry,
                    keep_geo_vars = geometry,
                    table=acstab,
                    year=year) |>
    mutate(geotype="state") |>
    rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))

  counties <- get_acs(geography="county",
                      state="NY",
                      # county=counties,
                      geometry = geometry,
                      keep_geo_vars = geometry,
                      table=acstab,
                      year=year) |>
    mutate(geotype="county") |>
    rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))
  # name.x is short, name.y has " County, New York"

  cousubs <- get_acs(geography="county subdivision",
                     state="NY",
                     # county=cosubs_county,
                     geometry = geometry,
                     keep_geo_vars = geometry,
                     table=acstab,
                     year=year) |>
    mutate(geotype="cousub") |>
    rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))
  # for cousubs, name.y is a full name, e.g.:
  #   name.x      Manhattan
  #   name.y      Manhattan borough, New York County, New York
  #   namelsad    Manhattan borough
  #   namelsadco  New York County


  places <- get_acs(geography="place",
                    state="NY",
                    table=acstab,
                    geometry = geometry,
                    keep_geo_vars = geometry,
                    year=year) |>
    mutate(geotype="place") |>
    rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))

  schools <- get_acs(geography="school district (unified)",
                     state="NY",
                     geometry = geometry,
                     table=acstab,
                     year=year) |>
    mutate(geotype="schooldist")  |>
    rename(any_of(c(NAME="NAME.x", FULLNAME="NAME.y")))

  # dfzip <- get_acs(geography="zcta",
  #               state="NY",
  #               # zcta=c("12816", "12834"),
  #               zcta=c(12816, 12834),
  #               table=acstab,
  #               year=2019) # doesn't work for 2020 or 2021

  df1 <- bind_rows(nation, states, counties, cousubs, places, schools)

  # names2 <- names(df1)
  # names2 <- str_replace(names2, "NAME.x", "NAME")

  df2 <- df1 |>
    as_tibble() |> # important so that we can work with columns
    lcnames() |>
    rename(basename=name) |>
    mutate(shortname=str_extract_before_first(basename, ","),
           endyear=!!year, table=acstab, survey="acs5") |>
    relocate(shortname, .after=fullname)

  # define variables to keep, depending on status of geometry
  # keepxgeo <- quote(c(geotype, year, geoid, geoname=name, variable, estimate, moe))
  # keepgeo <- quote(c(geotype, year, geoid, geoname=name, variable, estimate, moe, geometry))
  # if(geometry) {
  #   keepvars <- keepgeo
  #   } else keepvars <- keepxgeo

  # df <- df2 |>
  #   select(!!keepvars)

  return(df2)
}


popall <- read_acstab("B01003") |> # every geography
  select(GEO_ID, pop20215=B01003_E001)



# get acs geographies that correspond to those from acs newformat ---------
df1 <- get_acstab(acstab="B01003", year=2021, geometry=TRUE) # get a table with only one variable, for exploring names
glimpse(df1)

## drop geometry  ----
df2 <- df1 |>
  select(-geometry) |>
  rename(pop=estimate) |>
  select(-c(variable, moe, table))
glimpse(df2)
tmp <- count(df2, shortname, fullname)
saveRDS(df2, path(dxwalks, "acs_geocodes_raw.rds"))






# logical expressions for records to get ----
usgeoids <- c("0100000US", "0100001US", "0100043US") # US, US urban, US rural
usrec <- expression(GEO_ID %in% usgeoids)

staterec <- expression(SUMLEVEL=="040" & COMPONENT=="00") # 50 states, DC, PR

# ny records
state_urban_rural <- expression(SUMLEVEL=="040" & COMPONENT %in% c("01", "43"))
state_urban <- expression(SUMLEVEL=="040" & COMPONENT=="01")
state_rural <- expression(SUMLEVEL=="040" & COMPONENT=="43")
ny <- expression(STATE=="36")
county <- expression(SUMLEVEL=="050")
cousub <- expression(SUMLEVEL=="060")
place <- expression(SUMLEVEL=="160")
concity <- expression(SUMLEVEL=="170") # none in New York

schdist <- expression(SUMLEVEL %in% c("950", "960", "970"))

nyrec <- expression(eval(ny) &
                      (
                        eval(state_urban_rural) |
                          eval(county) |
                          eval(cousub) |
                          eval(place) |
                          eval(schdist)
                      ))
# eval(usrec) | eval(staterec) | eval(nyrec)

# temp <- geo1 |>
#   filter(eval(nyrec))
#
# count(geo1, SUMLEVEL)
#
# temp <- geo3 |>
#   filter(eval(schdist))


# url <- "https://www2.census.gov/programs-surveys/acs/summary_file/2021/table-based-SF/data/5YRData/acsdt5y2021-b15003.dat"
#
# ussumlevs <- c("010", "040")
# nysumlevs <- c("050", "060", "160", "170", "400", "410", "430")

# base table with all desired governments ---------------------------------
# fn <- "Geos20215YR.txt"
# geo1 <- vroom(fs::path(xdacs, fn)) # 622k rows
# ns(geo1) # 621k

geo2 <- geo1 |>
  filter(eval(usrec) | eval(staterec) | eval(nyrec))



