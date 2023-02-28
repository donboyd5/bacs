


# setup -------------------------------------------------------------------

library(devtools)
library(usethis)
source(here::here("data-raw", "libraries.r"))


# get data ----------------------------------------------------------------

load(file=here::here("data", "xwalkny_newformat.rda"))
load(file=here::here("data", "xwalkny_tidy.rda"))


# compare names -----------------------------------------------------------

glimpse(xwalkny_newformat)
glimpse(xwalkny_tidy)

ns(xwalkny_newformat)
ns(xwalkny_tidy)

intersect(names(xwalkny_newformat), names(xwalkny_tidy))
# "stabbr"    "affgeoid"  "fullname"  "shortname" "pop20215"

skim(xwalkny_newformat) # affgeoid is 100% complete, also fullname
skim(xwalkny_tidy) # affgeoid is missing for these

# test merge #1 to look for anomalies ----------------------------------------

test1 <- full_join(xwalkny_newformat |>
                      rename(stabbr_new=stabbr, shortname_new=shortname,
                             fullname_new=fullname,
                             pop_new=pop20215),
                   xwalkny_tidy |>
                      rename(stabbr_tidy=stabbr, shortname_tidy=shortname,
                             fullname_tidy=fullname,
                             pop_tidy=pop20215),
                   by = join_by(affgeoid))

nrow(xwalkny_newformat)
nrow(xwalkny_tidy)
nrow(test1) # an extra 12 records vs. newformat
count(test1, innew, intidy)

tmp1 <- test1 |> filter(innew & is.na(intidy)) # 16 records
#  2 US recs - urban, rural
#  2 NY recs - urban, rural
#  9 NY recs - County subdivisions not defined in 9 counties
#  3 NY recs - school district Remainder of New York, New York, 1 each for sumlevel 950, 960, 970 (elem, second, unified)

tmp2 <- test1 |> filter(intidy & is.na(innew)) # 12 records
#  9 NY recs - County subdivisions not defined, Cayuga County, New York -- geoid 3601100000 affgeoid is missing
#  3 NY recs
check <- xwalkny_tidy |> filter(geoid %in% tmp2$geoid) # affgeoid is missing for all of these
# geoid --> affgeoid: 3601100000 --> 0600000US3601100000, etc., for the 9 county subdivs
# 3699999 for all 3 sds --> 9500000US3699999, 9600000US3699999, 9700000US3699999

# affgeoid and sd pop:
# 9500000US3699999 19848473
# 9600000US3699999 19851725
# 9700000US3699999 266272

# test merge #2 to look for anomalies ----------------------------------------
# prep xwalkny_tidy
xwalkny_tidy2 <- xwalkny_tidy |>
  # uncomment the filter for testing purposes
  # filter(str_detect(fullname, "County subdivisions not defined") |
  #          geoid=="3699999") |>
  mutate(affgeoid=case_when(str_detect(fullname, "County subdivisions not defined") &
                              is.na(affgeoid) ~ paste0("0600000US", geoid),
                            geoid=="3699999" & pop20215==19848473 ~ "9500000US3699999",
                            geoid=="3699999" & pop20215==19851725 ~ "9600000US3699999",
                            geoid=="3699999" & pop20215==266272 ~ "9700000US3699999",
                            TRUE ~ affgeoid))

test2 <- full_join(xwalkny_newformat |>
                     rename(stabbr_new=stabbr, shortname_new=shortname,
                            fullname_new=fullname,
                            pop_new=pop20215),
                   xwalkny_tidy2 |>
                     rename(stabbr_tidy=stabbr, shortname_tidy=shortname,
                            fullname_tidy=fullname,
                            pop_tidy=pop20215),
                   by = join_by(affgeoid))


nrow(xwalkny_newformat)
nrow(xwalkny_tidy2)
nrow(test2) # same as newformat
count(test2, innew, intidy)

tmp <- test2 |> filter(innew & is.na(intidy)) # the 4 urban-rural records we expect

## check stabbr ----
# stabbr_tidy=stabbr, shortname_tidy=shortname, pop_tidy=pop20215
tmp <- test2 |>
  filter(stabbr_tidy != stabbr_new |
           is.na(stabbr_tidy) |
           is.na(stabbr_new)) # 17 recs
# stabbr_tidy is missing for the 4 urban-rurals, the US, and the 12 records corrected in step 1
# stabbr_new is good for all

## check shortname ----
tmp <- test2 |>
  filter(shortname_tidy != shortname_new |
           is.na(shortname_tidy) |
           is.na(shortname_new))  |>
  select(affgeoid, fullname_new, fullname_tidy, shortname_new, shortname_tidy)

# shortname_tidy does not have county, town, village, ... in its name

## check pop ----
tmp <- test2 |>
  filter(pop_tidy != pop_new |
           is.na(pop_tidy) |
           is.na(pop_new))
# only the 4 urban-rural records are missing pop_tidy

# test merge #3 to look for anomalies ----------------------------------------
# now:
#   drop tidy: stabbr, fullname, shortname, and pop20215
#   construct shortestname -- dropping the end of shortname

skim(xwalkny_newformat)

test3 <- full_join(xwalkny_newformat,
                   xwalkny_tidy2 |>
                     rename(statename=state_name) |>
                     select(-c(stabbr, fullname, shortname, pop20215)),
                   by = join_by(affgeoid))
glimpse(test3)
skim(test3)

# check the various fips codes
ns(xwalkny_newformat)
ns(xwalkny_tidy)

# new, tidy:
# county, countyfp; what is countyns??
# cousub, cousubfp, cousubns
# place, placefp, placens
# what are new sdelm, sduni

tmp <- test3 |>
  select(affgeoid, geotype, fullname, stabbr, state, statefp, pop20215) # drop statefp

tmp <- test3 |>
  select(affgeoid, geotype, fullname, county, countyfp, countyns, pop20215) # drop countyfp

tmp <- test3 |>
  filter(!is.na(cousub)) |>
  select(affgeoid, geotype, fullname, cousub, cousubfp, cousubns, pop20215) # drop cousubfp

tmp <- test3 |>
  filter(!is.na(place)) |>
  select(affgeoid, geotype, fullname, place, placefp, placens, pop20215) # drop placefp

tmp <- test3 |>
  # filter(!is.na(place)) |>
  select(affgeoid, geotype, fullname, sdelm, sduni, elsdlea, scsdlea, unsdlea, pop20215)
# we seem to be missing a new format sd secondary code, so keep the 3 lea codes from tidy



# final merge -------------------------------------------------------------

xwalkny <- full_join(xwalkny_newformat |>
                       select(-sdelm, -sduni), # temporary until we find out about the missing secondary code
                   xwalkny_tidy2 |>
                     rename(statename=state_name) |>
                     select(-c(stabbr, fullname, shortname, pop20215,
                               statefp,
                               countyfp, cousubfp, placefp)),
                   by = join_by(affgeoid)) |>
  mutate(shortestname=case_when(geotype %in% c("usrec", "state", "urban", "rural") ~ shortname,
                                geotype=="county" ~ str_remove(shortname, " County$"),
                                geotype=="cousub" & str_detect(shortname, " city$") ~ str_remove(shortname, " city$"),
                                geotype=="cousub" & str_detect(shortname, " town$") ~ str_remove(shortname, " town$"),
                                geotype=="cousub" & str_detect(shortname, " Reservation$") ~ str_remove(shortname, " Reservation$"),
                                geotype=="cousub" & str_detect(shortname, " borough$") ~ str_remove(shortname, " borough$"),
                                geotype=="place" & str_detect(shortname, " city$") ~ str_remove(shortname, " city$"),
                                geotype=="place" & str_detect(shortname, " village$") ~ str_remove(shortname, " village$"),
                                geotype=="place" & str_detect(shortname, " CDP$") ~ str_remove(shortname, " CDP$"),
                                TRUE ~ shortname)) |>
  relocate(affgeoid, geoid, fullname, shortname, shortestname, geotype, nygeotype) |>
  relocate(innew, .before=intidy) |>
  relocate(elsdlea, scsdlea, unsdlea, .after=place)

glimpse(xwalkny)
skim(xwalkny)

usethis::use_data(xwalkny, overwrite = TRUE)

# str_view("Cambridge town", " town$")
# str_remove("Cambridge town", " town$")
# str_detect("Cambridge town", " town$")
# x <- c("apple", "banana", "pear")
# str_view(x, "^a") # beginning
# str_view(x, "a$")

