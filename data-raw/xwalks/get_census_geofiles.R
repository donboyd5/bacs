

# geography codes and information ----

## Census sources ----
# https://www.census.gov/programs-surveys/acs/geography-acs.html
# https://www.census.gov/programs-surveys/geography/technical-documentation/code-lists.html

### Census reference materials ----
# https://www.census.gov/programs-surveys/acs/geography-acs/reference-materials.html
# Use the "Detailed list of geographic areas" files to see the name and GEOID of each geography available in each release



## tidycensus source ----
# https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus


# https://api.census.gov/data/2021/acs/acs5/geography.html


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
xdgeo <- r"(E:\data\acs\sf\geographies)"


# download Census geo files ------------------------------------------------------





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


# bad codes ?? ----
# 1600000US360415
