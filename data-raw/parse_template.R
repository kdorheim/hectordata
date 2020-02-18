# Read the template Hector input .ini file ("hector_template.ini") into a
# character vector, then store it as exported package data to be used when
# creating Hector .ini files for new scenarios
#
# Matt Nicholson
# 17 Feb 2020

# Use `devtools::load_all()` here instead of `library()` because this script is
# more likely to be run while developing the package, in which case the R
# package library may have an out of date version of Hector (e.g. with new
# variables missing).
devtools::load_all()

ini_template <- readLines("hector_template.ini")

usethis::use_data(ini_template, internal = TRUE, overwrite = TRUE)
