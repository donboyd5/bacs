
library(devtools)
library(usethis)
source(here::here("data-raw", "libraries.r"))
library(bacs)

df1 <- get_acstab("B01003")
df2 <- enhance(df)

df3 <- get_enhanced("B01001")

acsvars
df3a <- df3 |>
  left_join(acsvars |> select(variable, line, label),
            by = join_by(variable))

df3a |>
  filter(stabbr=="US", geotype=="usrec") |>
  select(affgeoid, fullname, variable, estimate, line, label)



df <- read_acstab("B01003")

fn <- "Geos20215YR.txt"
geo1 <- vroom(fs::path(bacs:::default_xdacs, fn))

# selected summary levels
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

geony <- geo1 |>
  filter(STUSAB=="NY") |>
  filter(SUMLEVEL %in% c("400", "410", "430")) |>
  left_join(df, join_by(GEO_ID))
