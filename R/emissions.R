# This file contains functions to construct Hector-readable emissions files
# from RCMIP emissions files.
#
# Matt Nicholson
# 28 Feb 2020

#' Generate a Hector input emissions file for a given RCMIP scenario
#'
#' @param scenario Character vector; Scenario to generate an emissions file for.
#' @return quien sabe
#' @author Matt Nicholson
#' @export
generate_emissions <- function(scenario) {
  hector_minyear <- 1745
  hector_maxyear <- 2100

  # Restrict inputs to the range of dates
  input_sub <- get_rcmip_inputs() %>%
    dplyr::filter(
      Scenario == !!scenario,
      year >= hector_minyear,
      year <= hector_maxyear
    )
  stopifnot(nrow(input_sub) > 0)

  minyear <- max(min(input_sub$year), hector_minyear)
  maxyear <- min(max(input_sub$year), hector_maxyear)
  rundates <- seq(minyear, maxyear)

  message("Not yet implemented!")
  message("Bro just copy+paste hector-rcmip::run-scenario::set_scenario()")
}

#' Long RCMIP inputs data.frame
#'
#' @param targetfile Character vector, optional; File to read RCMIP input from.
#' Default is inst/rcmip-inputs.fst
#' @return
#' @author Alexey Shiklomanov
#' @importFrom magrittr %>%
#' @export
get_rcmip_inputs <- function(targetfile = NULL) {
  if (is.null(targetfile)) {
    targetfile <- here::here("inst", "rcmip-inputs.fst")
  }
  stopifnot(file.exists(targetfile))
  fst::read_fst(targetfile) %>%
    tibble::as_tibble()
}

#' Extract emissions data for a given variable from the RCMIP emissions.
#'
#' @param hector_var Character vector; Name of the emission variable to retrieve,
#' in Hector format.
#' @return Quien sabe
#' @author Alexey Shiklomanov
#' @export
subset_hector_var <- function(input_data, hector_var) {
  hector_sub <- rcmip2hector_df() %>%
    dplyr::filter(hector_variable == !!hector_var)
  stopifnot(nrow(hector_sub) > 0)
  result <- input_data %>%
    dplyr::semi_join(hector_sub, c("Variable" = "rcmip_variable"))
  if (length(unique(result$Variable)) > 1) {
    stop("Multiple matching variables found for ", hector_var)
  }
  result
}
