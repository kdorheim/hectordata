# This file reads a Hector input .ini file template and creates a new input .ini
# for given CMIP6 scenario(s)
#
# Matt Nicholson
# 14 Feb 2020

#' Create a new Hector input .ini file for a given scenario. New .ini files are
#' written to hectordata/inst/input.
#'
#' @param scenario Character vector; Name of the scenario for which an input file is being created.
#' @param emission_file Character vector; Name of the emission file for the given scenario.
#' @return Character vector; Path of the new .ini file
#'
#' @example
#' create_scenario_ini("rcp45")
#'
#' TODO:
#'   * Add export tag?
create_scenario_ini <- function(scenario) {
  # Create the name of the emissions file corresponding to the scenario
  emissions_file <- parse_emission_fname(scenario)

  # Replace the "var_runName" placeholder with the scenario name
  scenario_ini <- gsub("var_runName", scenario, ini_template)

  # Replace the "var_emissionsPath" placeholder with the name of the emissions file
  scenario_ini <- gsub("var_emissionsPath", emissions_file, scenario_ini)

  ini_name <- parse_ini_fname(scenario)
  f_out <- file.path("..", "input",  ini_name)
  cat(scenario_ini, file=f_out, sep="\n")
  invisible(f_out)
}

#' Parse the name of the emissions file for a given scenario.
#'
#' The filename will be of the format: "<scenario>_emissions.csv".
#' Note: the letters in the scenario name will be converted to upper-case to follow
#' existing Hector input file naming conventions.
#'
#' @param scenario Character vector; Name of the scenario.
#' @return Character vector; Name of the emissions file corresponding to the scenario
#'
#' @example
#' parse_emission_fname("rcp45")
parse_emission_fname <- function(scenario) {
  # Cast to uppercase to follow existing Hector convention
  scen_upper <- toupper(scenario)
  f_name <- paste0(scen_upper, "_emissions.csv")
  f_path <- file.path("emissions", f_name)
}

#' Parse the filename of a Hector input .ini file for a given scenario
#'
#' The filename will be of the format: "hector_<scenario>.ini"
#'
#' @param scenario Character vector; Name of the scenario.
#' @return Character vector; Name of the emissions file corresponding to the scenario
#'
#' @example
#' parse_ini_fname("rcp45")
parse_ini_fname <- function(scenario) {
  ini_name <- paste0("hector_", scenario, ".ini")
}
