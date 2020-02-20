# This file contains functions that create a Hector .ini input file for a given
# scenario. The style of the created .ini files match that of the existing
# input .ini files found in the Hector R package.
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
#' @examples
#' create_scenario_ini("rcp45")
#'
#' @export
create_scenario_ini <- function(scenario) {
  input_dir <- file.path(system.file('input', package='hectordata'))
  # Create the name of the emissions file corresponding to the scenario
  emissions_file <- parse_emission_fname(scenario)
  # Replace placeholder strings in the template ini
  scenario_ini <- replace_ini_vars(scenario, emissions_file)
  # Construct new ini filename and path; write to file
  ini_name <- parse_ini_fname(scenario)
  ini_path <- file.path(input_dir, ini_name)
  write_file(scenario_ini, ini_path)
  invisible(ini_path)
}

#' Replace the placeholder variables in the template Hector .ini file with the
#' propper scenario variables
#'
#' Helper function for create_scenario_ini
#'
#' @param scenario Character vector; Name of the scenario for which an input file is being created.
#' @param emission_file Character vector; Name of the emission file for the given scenario.
#' @return Character vector; Lines of the scenario .ini
#' @keywords internal
#' @export
replace_ini_vars <- function(scenario, emissions_file) {
  # Replace the "var_runName" placeholder with the scenario name
  scenario_ini <- gsub("var_runName", scenario, ini_template)
  # Replace the "var_emissionsPath" placeholder with the name of the emissions file
  scenario_ini <- gsub("var_emissionsPath", emissions_file, scenario_ini)
  invisible(scenario_ini)
}

#' Write a character vector to file
#'
#' @param file_lines Character vector; Lines of the file to write
#' @param out_path Character vector; Path of the file to write
#' @keywords internal
#' @export
write_file <- function(file_lines, out_path) {
  writeLines(file_lines, con=out_path, sep="\n")
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
#' @examples
#' parse_emission_fname("rcp45")
#'
#' @keywords internal
#' @export
parse_emission_fname <- function(scenario) {
  # Cast to uppercase to follow existing Hector .ini conventions
  scen_upper <- toupper(scenario)
  f_name <- paste0("emissions/", scen_upper, "_emissions.csv")
  invisible(f_name)
}

#' Parse the filename of a Hector input .ini file for a given scenario
#'
#' The filename will be of the format: "hector_<scenario>.ini"
#'
#' @param scenario Character vector; Name of the scenario.
#' @return Character vector; Name of the emissions file corresponding to the scenario
#'
#' @examples
#' parse_ini_fname("rcp45")
#'
#' @keywords internal
#' @export
parse_ini_fname <- function(scenario) {
  ini_name <- paste0("hector_", scenario, ".ini")
  invisible(ini_name)
}
