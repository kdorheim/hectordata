# Functions to read raw data into internal package data
#
# Matt Nicholson
# 25 Feb 2020

# Use `devtools::load_all()` here instead of `library()` because this script is
# more likely to be run while developing the package, in which case the R
# package library may have an out of date version of Hector (e.g. with new
# variables missing).

devtools::load_all()

# Read the template Hector input .ini file
ini_template <- readLines("data-raw/hector_template.ini")

# Read the RCMIP to Hector emissions Look-Up Table
rcmip_emissions_lut <- readr::read_csv("data-raw/variable-conversion.csv",
                                       col_types = readr::cols(.default = "c"))

usethis::use_data(ini_template, rcmip_emissions_lut, internal = TRUE, overwrite = TRUE)


