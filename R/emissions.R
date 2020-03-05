# This file contains functions to construct Hector-readable emissions files
# from RCMIP emissions files.
#
# Matt Nicholson
# 28 Feb 2020

#' Generate a Hector input emissions file for a given RCMIP scenario
#'
#' @param scenario Character vector; Scenario to generate an emissions file for.
#' @param outpath Character vector, optional; Directory to write the created emissions file to.
#'   If not given, the file will be written to inst/input/emissions
#' @return quien sabe
#' @author Matt Nicholson
#' @export
generate_emissions <- function(scenario, outpath = NULL) {
  if (is.null(outpath)) {
    outpath <- file.path("inst", "input", "emissions")
    dir.create(outpath, showWarnings = FALSE, recursive = TRUE)
  }

  # Get the RCMIP to Hector variable LUT
  rcmip2hector_lut <- rcmip2hector_df()

  hector_minyear <- 1745
  hector_maxyear <- 2100

  # Restrict inputs to the range of dates
  # input_sub <- get_rcmip_inputs() %>%
  #   dplyr::filter(
  #     Scenario == !!scenario,
  #     year >= hector_minyear,
  #     year <= hector_maxyear
  #   )
  rcmip_inputs <- get_rcmip_inputs()
  input_sub <- filter_rcmip_inputs(rcmip_inputs, scenario, hector_minyear, hector_maxyear)
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

  # Variables that appear in Hector emission input files:
  #   Misc:
  #     BC_emissions,      C2F6_emissions,  C6F14,
  #     CCl4_emissions,    CF4_emissions,   CH4_emissions,
  #     CO_emissions,      N2O_emissions,   NH3,
  #     NMVOC_emissions,   NOX_emissions,   OC_emissions,
  #     SF6_emissions,     SO2_emissions,   SOx,
  #     ffi_emissions,     luc_emissions
  #   CH3's:
  #     CH3Br_emissions,  CH3CCl3_emissions, CH3Cl_emissions
  #   CFC's:
  #     CFC113_emissions, CFC114_emissions, CFC115_emissions,
  #     CFC11_emissions,  CFC12_emissions,
  #   Halons:
  #     halon1211_emissions, halon1301_emissions, halon2402_emissions,
  #     HALON1202
  #   HCF's:
  #     HCF141b_emissions, HCF142b_emissions, HCF22_emissions
  #   HFC's:
  #     HFC125_emissions,   HFC134a_emissions, HFC143a_emissions,
  #     HFC227ea_emissions, HFC23_emissions,   HFC245fa_emissions,
  #     HFC32_emissions,    HFC4310_emissions

  # Metadata column of the output Hector emissions dataframe
  output_meta_col <- get_meta_col(scenario, rundates)

  # CO2
  ffi <- subset_hector_var(input_sub, rcmip2hector_lut, "ffi_emissions")
  luc <- subset_hector_var(input_sub, rcmip2hector_lut, "luc_emissions")
  co2_conc <- subset_hector_var(input_sub, rcmip2hector_lut, "CO2_constrain")

  # TODO Instead of setting the Hector variable value, we need to write to csv

  if (nrow(ffi) && nrow(luc)) {
    # Use FFI and LUC emissions
    hc <- set_variable(hc, ffi, ...)
    hc <- set_variable(hc, luc, ...)
  } else if (nrow(co2)) {
    # Use CO2 concentrations
    # TODO So do we just leave ffi & luc blank in this case?
    hc <- set_variable(hc, co2, ...)
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
  ch4_emit <- subset_hector_var(input_sub, rcmip2hector_lut, "CH4_emissions")
  ch4_conc <- subset_hector_var(input_sub, rcmip2hector_lut, "CH4_constrain")
  if (nrow(emit)) {
    hc <- set_variable(hc, emit, ...)
  } else if (nrow(conc)) {
    hc <- set_variable(hc, conc, ...)
  }

  # OH and ozone
  nox_emit <- subset_hector_var(input_sub, rcmip2hector_lut, "NOX_emissions")
  co_emit  <- subset_hector_var(input_sub, rcmip2hector_lut, "CO_emissions")
  voc_emit <- subset_hector_var(input_sub, rcmip2hector_lut, "NMVOC_emissions")
  if (nrow(nox_emit) && nrow(co_emit) && nrow(voc_emit)) {
    # Only set these if all three are present
    hc <- set_variable(hc, nox_emit, ...)
    hc <- set_variable(hc, co_emit, ...)
    hc <- set_variable(hc, voc_emit, ...)
  }

  # N2O
  n2o_emit <- subset_hector_var(input_sub, rcmip2hector_lut, "N2O_emissions")
  n2o_conc <- subset_hector_var(input_sub, rcmip2hector_lut, "N2O_constrain")
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
    dat <- subset_hector_var(input_sub, rcmip2hector_lut, v)
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
#'   Default is inst/rcmip-inputs.fst
#' @return quien sabe
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

#' Filter RCMIP inputs dataframe by start year, end year, & scenario
#'
#' @param rcmip_inputs tibble/dataframe of RCMIP input data
#' @param scenario Character vector; Scenario of the emissions file being generated
#' @param year_min Integer; Minimum Hector year
#' @param year_max Integer; Maximum Hector year
#' @return filtered/subsetted RCMIP input dataframe, invisibly
filter_rcmip_inputs <- function(rcmip_inputs, scenario, year_min, year_max) {
  input_sub <- rcmip_inputs %>%
    dplyr::filter(
      Scenario == !!scenario,
      year >= year_min,
      year <= year_max
    )
  invisible(input_sub)
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
#'   in Hector format.
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

# TODO rename function ("stash_variable"?)
#' Set Hector variable to RCMIP data
#'
#' @param input_data `data.frame` of RCMIP inputs for a specific scenario.
#' @param hector_vars RCMIP to Hector variable conversion table
#' @param varname RCMIP variable name (optional). Defaults to unique `Variable` in
#'   `input_data`.
#' @param interpolate (Logical) If `TRUE` (default), interpolate incomplete time
#'   series using [stats::approxfun()]
#' @return `core`, invisibly
#' @author Alexey Shiklomanov, Matt Nicholson
#' @export
get_variable_col <- function(input_data, hector_vars,
                             varname = NULL,
                             interpolate = TRUE) {
  if (!(nrow(input_data) > 0)) {
    warning("Empty input data. Returning core unmodified.")
    return(core)
  }
  stopifnot(
    "Variable" %in% colnames(input_data),
    "year" %in% colnames(input_data),
    "value" %in% colnames(input_data)
  )
  if (is.null(varname)) varname <- unique(input_data[["Variable"]])
  stopifnot(length(unique(input_data[["Variable"]])) == 1)
  rundates <- seq(hector::startdate(core), hector::enddate(core))
  varconv <- dplyr::filter(hector_vars, rcmip_variable == !!varname)
  stopifnot(nrow(varconv) == 1)
  unit <- varconv$rcmip_udunits
  hector_unit <- varconv$hector_udunits
  hector_name <- varconv$hector_variable
  invar <- input_data %>%
    dplyr::filter(Variable == !!varname) %>%
    dplyr::arrange(year)
  if (interpolate) invar <- interpolate_var(invar)
  year <- invar$year
  value <- udunits2::ud.convert(invar$value, unit, hector_unit)
  var_col <- c("", "", varconv$hector_unit, hector_name, value)
  invisible(var_col)
}

#' Interpolate a Hector variable
#'
#' @param dat quien sabe
#' @return quien sabe
#' @author Alexey Shiklomanov
interpolate_var <- function(dat) {
  yrs <- sort(dat$year)
  if (any(diff(yrs) > 1)) {
    dat_l <- as.list(dat)
    dat_l$year <- seq(min(yrs), max(yrs))
    dat_l$value <- approxfun(yrs, dat$value)(dat_l$year)
    others <- !(names(dat_l) %in% c("year", "value"))
    dat_l[others] <- lapply(dat_l[others], unique)
    dat <- tibble::tibble(!!!dat_l)
  }
  dat
}

#' Get a list representing the metadata column of the output dataframe
#'
#' @param scenario Character vector; Scenario of the emissions file being generated
#' @param rundates Integer vector; Dates to generate emissions for
#' @return List representing the left-most column of the output emissions dataframe, invisibly
get_meta_col <- function(scenario, rundates) {
  # Metadata & date column for the output dataframe
  meta_col <- c(paste0("; ", scenario , " emissions"),
                paste0("; Produced by Hectordata"),
                ";UNITS:",
                "Date",
                rundates)
  invisible(meta_col)
}

# get_var_col <- function(){
#   var_col <- c("", "", <var_unit>, <var_name>, var_vals....)
#   invisible(var_col)
# }
