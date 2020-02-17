# Use `devtools::load_all()` here instead of `library()` because this script is
# more likely to be run while developing the package, in which case the R
# package library may have an out of date version of Hector (e.g. with new
# variables missing).
devtools::load_all()

ini_template <- readLines("hector_template.ini")

usethis::use_data(ini_template, internal = TRUE, overwrite = TRUE)
