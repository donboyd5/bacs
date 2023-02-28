
# Define constants to save to R/sysdata.rda

default_xdacs <- r"(E:\data\acs\sf\2021_5year)"

## AFTER xwalkny.rda has been updated resave default_geoids -----
load(file = here::here("data", "xwalkny_newformat.rda")) # newformat is Census Bureau's flat file table format
default_geoids <- xwalkny$GEO_ID
length(unique(default_geoids)) == length(default_geoids)


# save all constants defined above ----
usethis::use_data(default_xdacs, default_geoids, internal = TRUE, overwrite = TRUE)
