# This file contains functions to construct Hector-readable emissions files
# from RCMIP emissions files.
#
# Matt Nicholson
# 28 Feb 2020

#' Generate a Hector input emissions file for a given RCMIP scenario
#'
#' @param scenario Character vector; Scenario to generate an emissions file for.
#' @param outpath Character vector, optional; Directory to write the created emissions file to.
#' If not given, the file will be written to inst/input/emissions
#' @return quien sabe
#' @author Matt Nicholson
#' @export
generate_emissions <- function(scenario, outpath = NULL) {
  if (is.null(outpath)) {
    outpath <- file.path("inst", "input", "emissions")
    dir.create(outpath, showWarnings = FALSE, recursive = TRUE)
  }

  rcmip2hector_lut <- rcmip2hector_df()

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

  # Special case scenarios. These require inputs to be interpolated back to start date.
  abrupt_scenarios <- sprintf("abrupt-%sCO2", c("0p5x", "2x", "4x"))
  interp_scenarios <- c("piControl", "1pctCO2", "1pctCO2-4xext", abrupt_scenarios)

  if (scenario %in% interp_scenarios) {
    # HACK: Need to extend the time series to get this to work properly
    input_wide <- input_sub %>%
      dplyr::select(Variable, year, value) %>%
      tidyr::pivot_wider(names_from = "Variable", values_from = "value") %>%
      tidyr::complete(year = seq(hector_minyear, hector_maxyear)) %>%
      dplyr::mutate_if(
        is.double,
        ~approxfun(year, .x, rule = 2)(year)
      )
    if (scenario %in% abrupt_scenarios) {
      c0 <- hector::fetchvars(hc, hector::startdate(hc), "Ca")[["value"]]
      input_wide <- input_wide %>%
        dplyr::mutate(
          `Atmospheric Concentrations|CO2` = dplyr::case_when(
            year < min(input_sub$year) ~ c0,
            TRUE ~ `Atmospheric Concentrations|CO2`
          )
        )
    }
    input_sub <- input_wide %>%
      tidyr::pivot_longer(-year, names_to = "Variable", values_to = "value")
  }

  # CO2
  ffi <- subset_hector_var(input_sub, "ffi_emissions")
  luc <- subset_hector_var(input_sub, "luc_emissions")
  co2 <- subset_hector_var(input_sub, "CO2_constrain")

  if (nrow(ffi) && nrow(luc)) {
    # Use FFI and LUC emissions
    hc <- set_variable(hc, ffi, ...)
    hc <- set_variable(hc, luc, ...)
  } else if (nrow(co2)) {
    # Use CO2 concentrations
    hc <- set_variable(hc, co2, ...)
    if (min(co2$year) <= hector_minyear) {
      # Also set the pre-industrial value
      hector::setvar(hc, NA, hector::PREINDUSTRIAL_CO2(),
                     co2$value[co2$year == hector_minyear], "ppm")
    }
    maxco2 <- 3500
    if (any(co2$value > maxco2)) {
      maxyear <- co2 %>%
        dplyr::filter(value < maxco2) %>%
        dplyr::pull(year) %>%
        max()
      warning(
        "Some CO2 concentrations over ", maxco2, " ppm. ",
        "Truncating to year ", maxyear, "."
      )
    }
  } else {
    warning("Scenario ", scenario, " has no CO2 data.")
  }

  # CH4
  emit <- subset_hector_var(input_sub, "CH4_emissions")
  conc <- subset_hector_var(input_sub, "CH4_constrain")
  if (nrow(emit)) {
    hc <- set_variable(hc, emit, ...)
  } else if (nrow(conc)) {
    hc <- set_variable(hc, conc, ...)
  }

  # OH and ozone
  nox_emit <- subset_hector_var(input_sub, "NOX_emissions")
  co_emit <- subset_hector_var(input_sub, "CO_emissions")
  voc_emit <- subset_hector_var(input_sub, "NMVOC_emissions")
  if (nrow(nox_emit) && nrow(co_emit) && nrow(voc_emit)) {
    # Only set these if all three are present
    hc <- set_variable(hc, nox_emit, ...)
    hc <- set_variable(hc, co_emit, ...)
    hc <- set_variable(hc, voc_emit, ...)
  }

  # N2O
  emit <- subset_hector_var(input_sub, "N2O_emissions")
  conc <- subset_hector_var(input_sub, "N2O_constrain")
  if (nrow(emit)) {
    hc <- set_variable(hc, emit, ...)
  } else if (nrow(conc)) {
    hc <- set_variable(hc, conc, ...)
  }

  # Variables that can be handled naively
  # NOTE: All of these will assume a default value of zero
  naive_vars <- c(
    "Ftalbedo", "SO2_emissions", "SV",
    "BC_emissions", "OC_emissions"
  )
  for (v in naive_vars) {
    dat <- subset_hector_var(input_sub, v)
    if (nrow(dat) > 0) {
      tryCatch(
        hc <- set_variable(hc, dat, ...),
        error = function(e) {
          stop("Hit the following error on variable ", v, ":\n",
               conditionMessage(e))
        }
      )
    } else {
      warning("Scenario ", scenario, " has no data for ", v, ". ",
              "Using default value.")
    }
  }

  # Same logic for halocarbons.
  # HACK: For now, only use the ones already defined.
  halocarbons <- rcmip2hector_lut %>%
    dplyr::filter(grepl("_halocarbon", hector_component)) %>%
    dplyr::transmute(halocarbon = gsub("_halocarbon", "", hector_component),
                     halocarbon_rxp = paste0(halocarbon, "$")) %>%
    dplyr::distinct(halocarbon, halocarbon_rxp)

  halocarbon_dict <- input_sub %>%
    dplyr::distinct(Variable) %>%
    fuzzyjoin::regex_inner_join(halocarbons, c("Variable" = "halocarbon_rxp")) %>%
    dplyr::mutate(
      datatype = dplyr::case_when(
        grepl("^Atmospheric Concentrations", Variable) ~ "concentration",
        grepl("^Emissions", Variable) ~ "emissions",
        TRUE ~ "UNKNOWN"
      ) %>% factor(c("emissions", "concentration", "UNKNOWN")),
      hector_variable = paste(halocarbon, datatype, sep = "_")
    )

  # Prefer emissions, then concentration, then UNKNOWN
  halocarbon_dict <- halocarbon_dict %>%
    dplyr::group_by(halocarbon) %>%
    dplyr::arrange(datatype, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  for (i in seq_len(nrow(halocarbon_dict))) {
    irow <- halocarbon_dict[i,]
    i_rcmip_var <- irow[["Variable"]]
    i_hector_var <- irow[["hector_variable"]]
    indat <- input_sub %>%
      dplyr::filter(Variable == !!i_rcmip_var)
    tryCatch(
      hc <- set_variable(hc, indat, ...),
      error = function(e) {
        stop(
          "Error setting Hector variable ", i_hector_var,
          " / RCMIP variable ", i_rcmip_var, "."
        )
      }
    )
  }
  invisible(maxyear)
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

#' Read the RCMIP to Hector variable Look-Up Table (LUT)
#'
#' @return dataframe
#' @author Alexey Shiklomanov
rcmip2hector_df <- function() {
  lut_file <- here::here("inst", "variable-conversion.csv")
  lut <- readr::read_csv(lut_file, col_types = readr::cols(.default = "c"))
  invisible(lut)
}

#' Extract emissions data for a given variable from the RCMIP emissions.
#'
#' @param input_data Dataframe; RCMIP input emissions
#' @param var_lut Dataframe; RCMIP to Hector variable conversion information
#' @param hector_var Character vector; Name of the emission variable to retrieve,
#' in Hector format.
#' @return Quien sabe
#' @author Alexey Shiklomanov, Matt Nicholson
#' @export
subset_hector_var <- function(input_data, var_lut, hector_var) {
  hector_sub <- var_lut %>%
    dplyr::filter(hector_variable == !!hector_var)
  stopifnot(nrow(hector_sub) > 0)
  result <- input_data %>%
    dplyr::semi_join(hector_sub, c("Variable" = "rcmip_variable"))
  if (length(unique(result$Variable)) > 1) {
    stop("Multiple matching variables found for ", hector_var)
  }
  result
}
