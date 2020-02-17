# This file reads a Hector input .ini file template and creates a new input .ini
# for given CMIP6 scenario(s)
#
# Matt Nicholson
# 14 Feb 2020

f_in <- "C:\\Users\\nich980\\code\\temp\\hectordata\\template.ini"
scenario_name <- "ssp245"
emission_file <- "SSP245_emissions.csv"

#' Create a Hector input .ini file for a given scenario
#'
#' @param scenario String, name of the scenario for which an input file is being created
#' @param emission_file String, name of the emission file for the given scenario
#' @param template String, path of the template Hector .ini file
#'
#' TODO:
#'   * Add export tag
#'   * Remove template file param after it's loaded into package data
create_scenario_ini <- function(scenario, emission_file, template) {
  ini_template  <- readLines(f_in)
  scenario_ini <- gsub("var_emissionsPath", emission_file, ini_template)
  f_out <- paste0("hector_", scenario_name, ".ini")
  cat(scenario_ini, file=f_out, sep="\n")
}
