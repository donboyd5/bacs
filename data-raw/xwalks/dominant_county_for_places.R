
# about -------------------------------------------------------------------

# for each place in ny, identify dominant county based on 2021 5-year acs population



# setup -------------------------------------------------------------------

library(devtools)
library(usethis)
source(here::here("data-raw", "libraries.r"))
# load(here::here("R", "sysdata.rda"))


# constants ---------------------------------------------------------------

dxwalks <- here::here("data-raw", "xwalks")
xdacs <- r"(E:\data\acs\sf\2021_5year)"  # external directory for acs


# get data -----------------------------------------------------

## base table with all desired governments ----
fn <- "Geos20215YR.txt"
geo1 <- vroom(fs::path(xdacs, fn)) # 622k rows
ns(geo1) # 621k
# note: GEO_ID on this file is same as affgeoid on tidycensus


## countynames ----
countynames <- geo1 |>
  filter(STUSAB=="NY", SUMLEVEL=="050") |>
  select(GEO_ID, STUSAB, COUNTY, NAME) |>
  mutate(countyname=str_remove(NAME, ", New York"))
countynames


## popall ----
popall <- bacs::read_acstab("B01003") |> # every geography
  select(GEO_ID, pop20215=B01003_E001)

## places ----
places <- geo1 |>
  filter(STUSAB=="NY", SUMLEVEL=="160") |>
  filter(GEO_ID != "1600000US3651000") |> # drop NYC
  select(GEO_ID, STUSAB, PLACE, placename=NAME)


# logical expressions for records to get ----
# ny records
ny <- expression(STATE=="36")
county <- expression(SUMLEVEL=="050")
cousub <- expression(SUMLEVEL=="060")
place <- expression(SUMLEVEL=="160")
# concity <- expression(SUMLEVEL=="170") # none in New York
#
# schdist <- expression(SUMLEVEL %in% c("950", "960", "970"))

nyrec <- expression(eval(ny) &
                      (eval(county) |
                         eval(cousub) |
                         eval(place) |
                         eval(schdist)
                      ))


## placeparts ----
pparts1 <- geo1 |>
  filter(STUSAB=="NY", SUMLEVEL=="070") |>
  filter(!PLACE %in% c("99999", "51000")) |> # drop TOVs and NYC
  select(GEO_ID, STUSAB, COUNTY, PLACE, partname=NAME) |>
  # note that there can be multiple place-part records for a place, in different (or same) county
  left_join(countynames |> select(STUSAB, COUNTY, countyname), by = join_by(STUSAB, COUNTY)) |>
  left_join(places |> select(STUSAB, PLACE, placename), by = join_by(STUSAB, PLACE)) |>
  left_join(popall, by = join_by(GEO_ID))

skim(pparts1)

# skimout <- skim(pparts1)
# keep_vars <- skimout |>
#   filter(complete_rate > 0) |>
#   pull(skim_variable)
# keep_vars

## collapse to get pop by place-county, then mark primary county ----
pparts2 <- pparts1 |>
  # collapse by place, county to get the county pop for each placepart
  summarise(pop_part=sum(pop20215),
            .by=c(STUSAB, COUNTY, countyname, PLACE, placename)) |> # DO NOT include partname or it will include part records
  mutate(pop_place=sum(pop_part),
         primaryco=pop_part==max(pop_part),
         partpct=pop_part / pop_place,
         .by=c(STUSAB, PLACE))

pparts3 <- pparts2 |>
  filter(primaryco) |>
  # bring in GEO_ID for each place
  left_join(places |> select(-placename),
            by = join_by(STUSAB, PLACE))
skim(pparts3)

places |> filter(!GEO_ID %in% pparts3$GEO_ID)

saveRDS(pparts3, path(dxwalks, "nyplace_dominant_county.rds"))

