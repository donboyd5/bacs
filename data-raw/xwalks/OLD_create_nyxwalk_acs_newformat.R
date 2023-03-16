
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
load(here::here("R", "sysdata.rda"))


# constants ---------------------------------------------------------------

# xdacs <- default_xdacs # external directory for acs


# population data ---------------------------------------------------------

popall <- read_acstab("B01003") |> # every geography
  select(GEO_ID, pop20215=B01003_E001)


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
fn <- "Geos20215YR.txt"
geo1 <- vroom(fs::path(default_xdacs, fn)) # 622k rows
ns(geo1) # 621k

geo2 <- geo1 |>
  filter(eval(usrec) | eval(staterec) | eval(nyrec))



## put geotype and nygeotype on the file ----

# drop unnecessary columns
skimout <- skim(geo2)
skimout

keep_vars <- skimout |>
  filter(complete_rate > 0) |>
  pull(skim_variable)
keep_vars


geo3 <- geo2 |>
  select(all_of(keep_vars)) |>
  select(-US, -FILEID) |> # other vars not needed
  mutate(geotype=case_when(eval(usrec) ~ "usrec",
                           eval(staterec) ~ "state",
                           eval(state_urban) ~ "urban",
                           eval(state_rural) ~ "rural",
                           eval(county) ~ "county",
                           eval(cousub) ~ "cousub", # for NY, break into city, town
                           eval(place) ~ "place", # for NY, break into village, CDP
                           eval(schdist) ~ "schdist")
         ) |>
  # get county, cousub, place, and schdist names from elements of the name variable
  # note that we cannot get county names for places from this
  mutate(ss=str_split(NAME, ","),
         lss=purrr::map(ss, length) |> str_trim() |> unlist()) |>
  unnest_wider(col=ss, names_sep="") |>
  mutate(across(starts_with("ss"), str_trim),
         countyname=case_when(geotype=="county" ~ ss1,
                              geotype=="cousub" ~ ss2),
         cousubname=case_when(geotype=="cousub" ~ ss1),
         placename=case_when(geotype=="place" ~ ss1),
         schdistname=case_when(geotype=="schdist" ~ ss1)) |>
  # determine nygeotype for cousubs
  mutate(nygeotype=case_when(geotype=="cousub" &
                               str_detect(cousubname, "town") ~ "town",
                             geotype=="cousub" &
                               str_detect(cousubname, "city") ~ "city-cousub", # excludes NYC
                             geotype=="cousub" &
                               str_detect(cousubname, "borough") ~ "borough",
                             geotype=="cousub" &
                               str_detect(cousubname, "Reservation") ~ "reservation",
                             geotype=="cousub" &
                               str_detect(cousubname, "not defined") ~ "undefined-cousub",
                             geotype=="cousub" &
                               str_detect(cousubname, "Chautauqua Lake UT") ~ "unorg-cousub",
                             TRUE ~ geotype)) |>
  mutate(nygeotype=case_when(geotype=="place" &
                               str_detect(placename, "village") ~ "village",
                             geotype=="place" &
                               str_detect(placename, "CDP") ~ "CDP",
                             geotype=="place" &
                               str_detect(placename, "city") ~ "city", # includes NYC
                             TRUE ~ nygeotype)) |>
  select(-c(lss, ss1, ss2, ss3))

count(geo3, geotype)
count(geo3 |> filter(geotype=="cousub"), nygeotype)
tmp <- geo3 |> filter(geotype=="cousub", is.na(nygeotype))

count(geo3 |> filter(geotype=="place"), nygeotype)
tmp <- geo3 |> filter(geotype=="place", is.na(nygeotype))
tmp <- geo3 |> filter(geotype=="place", nygeotype=="city")

count(geo3, geotype, nygeotype)

## get county names for later ----
countynames <- geo3 |>
  filter(geotype=="county") |>
  select(GEO_ID, STUSAB, COUNTY, countyname)


## bring in population ---
geo4 <- geo3 |>
  left_join(popall |>
              filter(GEO_ID %in% default_geoids),
            by = join_by(GEO_ID))

skim(geo4)

# determine primary county for places ----

## placeparts ----

pparts1 <- geo1 |>
  filter(STUSAB=="NY", SUMLEVEL=="070") |>
  left_join(popall, by = join_by(GEO_ID))

skimout <- skim(pparts1)

keep_vars <- skimout |>
  filter(complete_rate > 0) |>
  pull(skim_variable)
keep_vars

## collapse to get pop by place-county, then mark primary county ----
pparts2 <- pparts1 |>
  select(all_of(keep_vars)) |>
  filter(!PLACE %in% c("99999", "51000")) |> # drop TOVs and NYC
  # collapse by place, county to get the county pop for each placepart
  summarise(pop_part=sum(pop20215),
            .by=c(STUSAB, COUNTY, PLACE)) |>
  mutate(pop_place=sum(pop_part),
         primaryco=pop_part==max(pop_part),
         partpct=pop_part / pop_place,
         .by=c(STUSAB, PLACE)) |>
  left_join(countynames, by = join_by(STUSAB, COUNTY))

pparts3 <- pparts2 |>
  filter(primaryco)

## merge dominant county back with the main geo data ----
geo5 <- geo4 |>
  left_join(pparts3 |>
              select(STUSAB, PLACE,
                     county_primary=COUNTY,
                     coname_primary=countyname),
            by = join_by(STUSAB, PLACE))

## final cleanup of names ----
geo6 <- geo5 |>
  mutate(COUNTY=case_when(is.na(COUNTY) &
                            geotype=="place" &
                            !is.na(county_primary) ~ county_primary,
                          TRUE ~ COUNTY),
         countyname=case_when(is.na(countyname) &
                                geotype=="place" &
                                !is.na(coname_primary) ~ coname_primary,
                              TRUE ~ countyname))

geo6 |> filter(geotype=="place", is.na(COUNTY)) # NYC, as expected
geo6 |> filter(geotype=="place", is.na(countyname)) # NYC, as expected

count(geo6, geotype)


# conform names to tidycensus approach ------------------------------------
names(geo6)
ns(geo6)


geo7 <- geo6 |>
  lcnames() |>
  rename(affgeoid=geo_id, stabbr=stusab, fullname=name)

xwalkny_newformat <- geo7 |>
  select(-county_primary, -coname_primary) |>
  mutate(shortname=case_when(geotype %in% c("usrec", "urban", "rural", "state") ~
                               fullname,
                             geotype=="county" ~ countyname,
                             geotype=="cousub" ~ cousubname,
                             geotype=="place" ~ placename,
                             geotype=="schdist" ~ schdistname,
                             TRUE ~ NA_character_)) |>
  relocate(tl_geo_id, geotype, nygeotype, .after=affgeoid) |>
  relocate(shortname, .after=fullname) |>
  mutate(innew=TRUE)
glimpse(xwalkny_newformat)
skim(xwalkny_newformat)

usethis::use_data(xwalkny_newformat, overwrite = TRUE)


