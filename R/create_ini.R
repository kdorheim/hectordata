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
#' @param outpath Character vector, optional; Path of the directory to place the newly-created .ini file. The
#' default directory is hectordata/inst/input.
#' @param check_em Boolean, optional; Whether or not to check for the existence of the scenario's
#' emissions file before creating the scenario .ini file. If the emissions file is not found
#' in the correct directory (input/emissions), an error is raised. Default is TRUE.
#' @return Character vector; Path of the new .ini file
#'
#' @export
create_scenario_ini <- function(scenario, outpath = NULL, check_em = TRUE) {
  if (is.null(outpath)) {
    outpath <- file.path("inst", "input")
    dir.create(outpath, showWarnings = FALSE, recursive = TRUE)
  }
  # Create the name of the emissions file corresponding to the scenario
  emissions_file <- parse_emission_fname(scenario)
  # Check for the existence of the emissions file, if needed
  if (check_em) {
    check_emissions_file(scenario, emissions_file, outpath)
  }
  # Replace placeholder strings in the template ini
  scenario_ini <- replace_ini_vars(scenario, emissions_file)
  # Construct new ini filename and path; write to file
  ini_name <- parse_ini_fname(scenario)
  ini_path <- file.path(outpath, ini_name)
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
#' @param prefix Boolean, default is TRUE; If TRUE, prepend "emissions/" to the
#' parsed emissions file name. If FALSE, return only the emissions file name.
#' @return Character vector; Name of the emissions file corresponding to the scenario
#'
#' @keywords internal
#' @export
parse_emission_fname <- function(scenario, prefix = TRUE) {
  # Cast to uppercase to follow existing Hector .ini conventions
  scen_upper <- toupper(scenario)
  if (prefix) {
    f_name <- paste0("emissions/", scen_upper, "_emissions.csv")
  } else {
    f_name <- paste0(scen_upper, "_emissions.csv")
  }
  invisible(f_name)
}

#' Parse the filename of a Hector input .ini file for a given scenario
#'
#' The filename will be of the format: "hector_<scenario>.ini"
#'
#' @param scenario Character vector; Name of the scenario.
#' @return Character vector; Name of the emissions file corresponding to the scenario
#'
#' @keywords internal
#' @export
parse_ini_fname <- function(scenario) {
  ini_name <- paste0("hector_", scenario, ".ini")
  invisible(ini_name)
}

#' Check the Hectordata input directory to see if the emissions .csv file
#' corresponding to the given scenario exists. If the emissions file is not
#' found, an error is raised.
#'
#' @param scenario Character vector; Name of the scenario.
#' @param emission_fname Character vector; Name of the emission file for the given scenario,
#' with prepended "emissions/"
#' @param input_dir Character vector; Path of the root input directory
#' @keywords internal
#' @export
check_emissions_file <- function(scenario, emission_fname, input_dir) {
  em_path <- file.path(input_dir, emission_fname)
  if (!file.exists(em_path)) {
    err_msg <- paste0('Scenario emissions file not found: ', em_path)
    # stop(err_msg)
    message(err_msg)
  }
}
