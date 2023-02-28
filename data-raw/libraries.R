
library(rlang)
library(tidyverse)
tprint <- 75  # default tibble print
options(tibble.print_max = tprint, tibble.print_min = tprint) # show up to tprint rows

library(fs)

# data tools
library(vroom)
library(readxl)
library(openxlsx) # for writing xlsx files
library(lubridate)
# library(RColorBrewer)
library(RcppRoll)
# library(fredr)
library(skimr)

# census tools
library(censusapi)
library(tidycensus)

# boyd libraries
library(btools)
library(bdata)
library(bggtools)
library(bmaps)
