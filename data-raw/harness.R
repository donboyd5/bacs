# https://r-pkgs.org/whole-game.html



# setup -------------------------------------------------------------------

library(devtools)
# periodically do the following for test driving:
#  modify a function
#  load_all()

library(usethis)
source(here::here("data-raw", "libraries.r"))
library(bacs)
# load(here::here("R", "sysdata.rda"))
# load("R/sysdata.rda")



# miscellaneous techniques ------------------------------------------------

# use_r("strsplit1")
# load_all()
# use_test()

# THIS WILL CREATE A NEW README AND WIPE OUT THE EXISTING ONE SO BE CAREFUL!
# if (FALSE) {
#   use_readme_rmd()
#   use_readme_md()
# }

# after changing README.Rmd, run this to create the md file.
# devtools::build_readme()
rmarkdown::render('E:/R_projects/packages/bacs/README.Rmd',  encoding = 'UTF-8')
# devtools::build_readme(path="E:/R_projects/packages/bacs/inst") # re-render readme


# https://r-pkgs.org/man.html
# To summarize, there are four steps in the basic roxygen2 workflow:
#
#   Add roxygen2 comments to your .R files.
#   Run devtools::document() or press Ctrl/Cmd + Shift + D to convert roxygen2 comments to .Rd files.
#   Preview documentation with ?function.
#   Rinse and repeat until the documentation looks the way you want.




# https://stackoverflow.com/questions/32994977/how-to-create-a-constant-inside-r-package
# When building a package, you can use the command devtools::use_data(constants,
# pkg, internal = TRUE), to save the variable constants, while ensuring it's not
# available to the package users

# https://r-pkgs.org/data.html
# https://r-pkgs.org/data.html#sec-data-sysdata

# The easiest way to create R/sysdata.rda is to use usethis::use_data(internal = TRUE):
# internal_this <- ...
# internal_that <- ...
# usethis::use_data(internal_this, internal_that, internal = TRUE)

# put the use_data statements where the files are created, not here
# usethis::use_data(tables, overwrite = TRUE)
# usethis::use_data(tabvars, overwrite = TRUE)

