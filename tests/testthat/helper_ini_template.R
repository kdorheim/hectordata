fetch_hector_ini <- function(scenario) {
  hector_inputdir <- system.file('input', package='hector')
  hector_ini_name <- parse_ini_fname(scenario)
  hector_ini_path <- system.file(hector_inputdir, hector_ini_name)
  hector_ini      <- readLines( file(hector_ini_path, open="w+") )
  invisible(hector_ini)
}

get_emission_lines <- function(ini) {
  lines <- ini[grepl("*_emissions:csv:*", ini)]
  invisible(lines)
}

get_run_name <- function(ini) {
  name <- ini[grepl("run_name", ini)]
  invisible(name)
}
