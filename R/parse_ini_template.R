# This file reads a Hector input .ini file template and creates a new input .ini
# for given CMIP6 scenario(s)
#
# Matt Nicholson
# 14 Feb 2020

#' Create a new Hector input .ini file for a given scenario. New .ini files are
#' written to hectordata/inst/input.
#'
#' @param scenario String, name of the scenario for which an input file is being created.
#' @param emission_file String, name of the emission file for the given scenario.
#' @return None, writes a new file.
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

  ini_name <- paste0("hector_", scenario, ".ini")
  f_out <- file.path("..", "input",  ini_name)
  cat(scenario_ini, file=f_out, sep="\n")
}


#' Parse the name of the emissions file for a given scenario.
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
}
