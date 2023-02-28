
# this creates table data frame based only on the 2021 5-year ACS


load("R/sysdata.rda")
source(here::here("data-raw", "libraries.r"))


## ONETIME get 20215 table shells ----
df1 <- vroom(path(default_xdacs, "ACS20215YR_Table_Shells.txt"))
glimpse(df1)

## table variables ----
acsvars <- df1 |>
  rename(table=`Table ID`,
         variable=`Unique ID`) |>
  lcnames()
glimpse(acsvars)
# saveRDS(acsvars, path(default_xdacs, "acsvars.rds"))
# usethis::use_data(acsvars, overwrite = TRUE)


## tables ----
acstables <- acsvars |>
  select(table, title, universe) |>
  distinct()
glimpse(acstables)
# saveRDS(acstables, path(default_xdacs, "acstables.rds"))
# usethis::use_data(acstables, overwrite = TRUE)


# multi-year variable labels based on tidycensus ----

data(acsvars)

# get 5-year acs variables using tidy census ----
vacs2009 <- load_variables(2009, "acs5", cache = TRUE)
vacs2011 <- load_variables(2011, "acs5", cache = TRUE)
vacs2014 <- load_variables(2014, "acs5", cache = TRUE)
vacs2016 <- load_variables(2016, "acs5", cache = TRUE)
vacs2018 <- load_variables(2018, "acs5", cache = TRUE)
vacs2019 <- load_variables(2019, "acs5", cache = TRUE)
vacs2021 <- load_variables(2021, "acs5", cache = TRUE)
vacs1 <-
  bind_rows(vacs2009 %>% mutate(year=2009),
            vacs2011 %>% mutate(year=2011),
            vacs2014 %>% mutate(year=2014),
            vacs2016 %>% mutate(year=2016),
            vacs2018 %>% mutate(year=2018),
            vacs2019 %>% mutate(year=2019),
            vacs2021 %>% mutate(year=2021)) |>
  mutate(dataset="acs5") |>
  select(-geography) |>
  rename(title=concept)

vacs <- vacs1 |>
  rename(variable=name) |>
  mutate(table=str_extract_before_first(variable, "_"),
         label=str_extract_after_last(label, "!!") |>
           str_to_lower() |>
           str_remove(":")) |>
  arrange(year, table, variable) |>
  mutate(line=row_number(), .by=c(table, year)) |>
  relocate(year, table, line)

acsvarstc <- vacs
usethis::use_data(acsvarstc, overwrite = TRUE)


acstablestc <- acsvarstc |>
  select(year, table, title, dataset) |>
  distinct()
usethis::use_data(acstablestc, overwrite = TRUE)


acstabs <- acsshells |>
  summarise(n=n(), .by=c(table, year, title, universe))


