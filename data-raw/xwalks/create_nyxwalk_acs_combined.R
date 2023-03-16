


# setup -------------------------------------------------------------------

library(devtools)
library(usethis)
source(here::here("data-raw", "libraries.r"))


# get data ----------------------------------------------------------------

nyplaces <- readRDS(fs::path(dxwalks, "nyplace_dominant_county.rds"))
xwalk_base <- readRDS(fs::path(dxwalks, "xwalk_base.rds"))


# compare names -----------------------------------------------------------

glimpse(nyplaces)
glimpse(xwalk_base)

ns(nyplaces)
ns(xwalk_base)

skim(nyplaces) # affgeoid is 100% complete, also fullname
skim(xwalk_base) # affgeoid is missing for these


# merge -------------------------------------------------------------------
xwalk1 <- xwalk_base |>
  left_join(nyplaces |>
              select(affgeoid=GEO_ID, placecounty=COUNTY, placecountyname=countyname, placename),
            by = join_by(affgeoid)) |>
  mutate(countyfp=ifelse(geotype=="place", placecounty, countyfp),
         countyname=ifelse(geotype=="place", placecountyname, countyname))

glimpse(xwalk1)
skim(xwalk1)
count(xwalk1, geotype)

tmp <- xwalk1 |>
  filter(geotype=="place") |>
  filter(year==2011) |>
  select(affgeoid, contains("name"), geotype, year, pop5yr)

# 2021 all but nyc
# 2016 not some CDPs, mostly
# 2011 not some CDPs, mostly

xwalk <- xwalk1 |>
  select(-c(placecounty, placecountyname, placename)) |>
  select(affgeoid, geoid, year, stabbr, fullname, shortname, shortestname, geotype, nygeotype, pop5yr, popmoe, aland, awater, dataset, everything())

usethis::use_data(xwalk, overwrite = TRUE)


