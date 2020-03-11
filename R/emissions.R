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
generate_emissions <- function(scenario, outpath = NULL, debug = FALSE) {
  if (is.null(outpath)) {
    outpath <- file.path("inst", "input", "emissions")
  }
  dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

  hector_minyear <- 1745
  hector_maxyear <- 2100

  rcmip2hector_lut <- rcmip2hector_df()

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

  # Metadata column of the output Hector emissions dataframe
  # output_meta_col <- get_meta_col(scenario, rundates)

  # --- CO2 ---
  ffi <- subset_hector_var(input_sub, rcmip2hector_lut, "ffi_emissions")
  luc <- subset_hector_var(input_sub, rcmip2hector_lut, "luc_emissions")
  co2_conc <- subset_hector_var(input_sub, rcmip2hector_lut, "CO2_constrain")

  if (nrow(ffi) && nrow(luc)) {
    # Use FFI and LUC emissions
    output_meta_col <- get_meta_col(scenario, ffi$year)
    var_col <- get_variable_col(ffi, rcmip2hector_lut)
    output_matrix <- lists_2_matrix(output_meta_col, var_col)
    var_col <- get_variable_col(luc, rcmip2hector_lut)
    output_matrix <- add_list_2_matrix(output_matrix, var_col)
  } else if (nrow(co2)) {
    # Use CO2 concentrations
    output_meta_col <- get_meta_col(scenario, co2_conc$year)
    var_col <- get_variable_col(co2_conc, rcmip2hector_lut)
    output_matrix <- lists_2_matrix(output_matrix, var_col)
    maxco2 <- 3500
    if (any(co2_conc$value > maxco2)) {
      maxyear <- co2_conc %>%
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

  # --- CH4 ---
  emit <- subset_hector_var(input_sub, rcmip2hector_lut, "CH4_emissions")
  conc <- subset_hector_var(input_sub, rcmip2hector_lut, "CH4_constrain")
  if (nrow(emit)) {
    output_matrix <- process_var(emit, rcmip2hector_lut, output_matrix)
  } else if (nrow(conc)) {
    output_matrix <- process_var(emit, rcmip2hector_lut, output_matrix)
  }

  # --- OH and ozone ---
  nox_emit <- subset_hector_var(input_sub, rcmip2hector_lut, "NOX_emissions")
  co_emit  <- subset_hector_var(input_sub, rcmip2hector_lut, "CO_emissions")
  voc_emit <- subset_hector_var(input_sub, rcmip2hector_lut, "NMVOC_emissions")
  if (nrow(nox_emit) && nrow(co_emit) && nrow(voc_emit)) {
    # Only set these if all three are present
    output_matrix <- process_var(nox_emit, rcmip2hector_lut, output_matrix)
    output_matrix <- process_var(co_emit, rcmip2hector_lut, output_matrix)
    output_matrix <- process_var(voc_emit, rcmip2hector_lut, output_matrix)
  }

  # --- N2O ---
  emit <- subset_hector_var(input_sub, rcmip2hector_lut, "N2O_emissions")
  conc <- subset_hector_var(input_sub, rcmip2hector_lut, "N2O_constrain")
  if (nrow(emit)) {
    output_matrix <- process_var(emit, rcmip2hector_lut, output_matrix)
  } else if (nrow(conc)) {
    output_matrix <- process_var(emit, rcmip2hector_lut, output_matrix)
  }

  # --- Variables that can be handled naively ---
  # NOTE: All of these will assume a default value of zero
  naive_vars <- c(
    "Ftalbedo", "SO2_emissions", "SV",
    "BC_emissions", "OC_emissions"
  )
  for (v in naive_vars) {
    dat <- subset_hector_var(input_sub, rcmip2hector_lut, v)
    if (nrow(dat) > 0) {
      tryCatch(
        output_matrix <- process_var(dat, rcmip2hector_lut, output_matrix),
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
      output_matrix <- process_var(indat, rcmip2hector_lut, output_matrix),
      error = function(e) {
        stop(
          "Error setting Hector variable ", i_hector_var,
          " / RCMIP variable ", i_rcmip_var, "."
        )
      }
    )
  }
  if (!debug) {
    matrix_to_csv(output_matrix, scenario, outpath)
    invisible(outpath)
  } else {
    # For testing/debugging, only return the emissions matrix
    invisible(output_matrix)
  }
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
#' @author Matt Nicholson
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

#' Get a list representing the metadata column of the output dataframe
#'
#' @param scenario Character vector; Scenario of the emissions file being generated
#' @param rundates Integer vector; Dates to generate emissions for
#' @return List representing the left-most column of the output emissions dataframe, invisibly
#' @author Matt Nicholson
get_meta_col <- function(scenario, rundates) {
  yrs <- interpolate_years(rundates)
  meta_col <- c(paste0("; ", scenario , " emissions"),
                paste0("; Produced by Hectordata R package"),
                ";UNITS:",
                "Date",
                yrs)
  invisible(meta_col)
}

#' Create an emissions file column for a given variable
#'
#' @param input_data `data.frame` of RCMIP inputs for a specific scenario.
#' @param hector_vars RCMIP to Hector variable conversion table
#' @param varname RCMIP variable name (optional). Defaults to unique `Variable` in
#'   `input_data`.
#' @param interpolate (Logical) If `TRUE` (default), interpolate incomplete time
#'   series using [stats::approxfun()]
#' @return List representation of the variable matrix column
#' @author Alexey Shiklomanov, Matt Nicholson
#' @export
get_variable_col <- function(input_data, hector_vars,
                             varname = NULL,
                             interpolate = TRUE) {
  if (!(nrow(input_data) > 0)) {
    warning("Empty input data. Returning -1.")
    return(-1)
  }
  stopifnot(
    "Variable" %in% colnames(input_data),
    "year" %in% colnames(input_data),
    "value" %in% colnames(input_data)
  )
  if (is.null(varname)) varname <- unique(input_data[["Variable"]])
  var_len <- length(unique(input_data[["Variable"]]))
  if (var_len != 1) {
    err_msg <- paste0('Length of unique(input_data[["Variable"]]) is ', var_len, ', expected 1. Var: ', varname)
    stop(err_msg)
  }
  varconv <- dplyr::filter(hector_vars, rcmip_variable == !!varname)
  if (nrow(varconv) != 1) {
    err_msg <- paste0('nrow(varconv) != 1. Var: ', varname)
    stop(err_msg)
  }
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

#' Interpolate a range of years
#'
#' @param yrs List of years to interpolate
#' @return List of years
#' @author Matt Nicholson
interpolate_years <- function(yrs) {
  if (any(diff(yrs) > 1)) {
    ret_val <- seq(min(yrs), max(yrs))
  } else {
    ret_val <- yrs
  }
  invisible(ret_val)
}

#' Wrapper for get_variable_col() and add_list_2_matrix()
#'
#' @param input_data `data.frame` of RCMIP inputs for a specific scenario.
#' @param hector_vars RCMIP to Hector variable conversion table
#' @param output_matrix Emissions matrix
#' @param varname RCMIP variable name (optional). Defaults to unique `Variable` in
#'   `input_data`.
#' @param ... Parameters to pass on to add_list_2_matrix() (e.i., interpolate)
#' @return emissions matrix
#' @author Matt Nicholson
#' @export
process_var <- function(input_data, hector_vars, output_matrix,
                        varname = NULL, ...) {
  args <- list(...)
  if (is.null((args$interpolate))) { args$interpolate = TRUE }
  var_col  <- get_variable_col(input_data, hector_vars, varname=varname, interpolate=args$interpolate)
  out_matr <- add_list_2_matrix(output_matrix, var_col)
}

#' Combine two lists into a matrix. The lists are treated as column vectors
#'
#' @param list1
#' @param list2
#' @return a matrix
#' @author Matt Nicholson
lists_2_matrix <- function(list1, list2) {
  ret_val <- cbind(list1, list2)
  invisible(ret_val)
}

#' Add a list to a matrix as a new column
#'
#' @param matr Matrix
#' @param lst List to add to the matrix
#' @return matrix
#' @author Matt Nicholson
add_list_2_matrix <- function(matr, lst) {
  matr <- cbind(matr, lst)
  invisible(matr)
}

#' Write the emissions matrix to csv
#'
#' @param output_matrix Matrix containing emissions data
#' @param scenario Scenario corresponding to the emissions data
#' @param outpath Path of the directory to write the output (optional). Default
#'   path is inst/input/emissions.
#' @return Absolute path of the output csv file (invisibly)
#' @author Matt Nicholson
matrix_to_csv <- function(output_matrix, scenario, outpath = NULL) {
  if (is.null(outpath)) {
    outpath <- file.path("inst", "input", "emissions")
    dir.create(outpath, showWarnings = FALSE, recursive = TRUE)
  }
  # Create the filename of the output emissions file
  scenario <- toupper(scenario)
  scenario <- gsub("_", "", scenario)
  f_name <- paste0(scenario, "_emissions.csv")
  f_path <- file.path(outpath, f_name)
  write.table(output_matrix, file=f_path, sep=",", quote=FALSE, col.names=FALSE,
              row.names=FALSE)
  invisible(f_path)
}
