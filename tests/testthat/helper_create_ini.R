# Helper functions for tests in test_create_ini.R
# Matt Nicholson
# 20 Feb 2020

get_emission_lines <- function(ini) {
  lines <- ini[grepl("*_emissions:csv:*", ini)]
  invisible(lines)
}

get_run_name <- function(ini) {
  name <- ini[grepl("run_name", ini)]
  invisible(name)
}

read_test_ini <- function() {
  test_ini  <- file.path("hector_test_rcp45.ini")
  ini_lines <- readLines( file(test_ini, "r") )
  invisible(ini_lines)
}
