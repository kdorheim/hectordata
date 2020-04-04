# Script that converts the RCMIP inputs to Hector relevant units.

# TODO:
# 1. Do we want to include a CMIP5 scenario as a test to make sure that the unit
#    conversions are happening correctly? In the first submission to RCMIP this was and issue.
# 2. What are we going to do about the fact that the .fst file comes from the.
# 3. What do we want to make the naming and strucutre consistent with the Hector files or with
#    the CMIP files?
# Othere are several other function sepcific TODOs that may help with the enhancement
# of this script.

# 0. Set Up ---------------------------------------------------------------------
library(magrittr)

INPUT_DIR <- here::here('data-raw', 'CMIP6')

# Define helpful functions

# Remove spaces from a column of a data frame
# Inputs
#   df: a tibble or data frame
#   cols: a vector of columns in the data frame to remove the spaces from.
# Returns: a data frame of the same dimensions but have remoced all of the " " spaces from
#          the speceified column.
remove_spaces <- function(df, cols){

  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(all(cols %in% names(df)))

  lapply(cols, function(col){

    assertthat::assert_that(is.character(df[[col]]))
    df[[col]] <<- gsub(pattern = ' ', replacement = '', x = df[[col]])

  })

  df

}


# 1. Import Data  ---------------------------------------------------------------------
# Import the rcmip inputs, note that this file might be updated on https://www.rcmip.org/,
# was most likely created by https://github.com/ashiklom/hector-rcmip and that the RCMIP
# community might update the files, and if that is the case the rcmip-inputs.fst will
# also need to be updated.
# TODO modify this so that it pulls the csv file from the web site, this will ensure that
# the inputs are up to date and do not depend on the hector-rcmip pacakge. Because that is
# unideal.
raw_inputs <- tibble::as_tibble(fst::read_fst(file.path(INPUT_DIR, 'rcmip-inputs.fst')))
raw_inputs <- remove_spaces(raw_inputs, cols = "Variable")

# Read in the csv file that will be used to convert from the rcmip units to the hector units.
conversion_df <- read.csv(file.path(INPUT_DIR, "variable-conversion.csv"), stringsAsFactors = FALSE)
conversion_df <- remove_spaces(conversion_df, cols = c("hector_variable", "rcmip_variable"))

# The CMIP6 scenarios consist of the phase specific and DECK scenarios.
phase6_scns <- c("ssp370", "ssp370-lowNTCF", "ssp434", "ssp460",
                 "ssp119", "ssp126", "ssp245", "ssp534-over",
                 "ssp585")
DECK_scns   <- c("abrupt-0p5xCO2", "abrupt-2xCO2", "abrupt-4xCO2",
                 "historical", "piControl", "esm-bell-1000PgC",
                 "esm-bell-2000PgC", "esm-bell-750PgC", "esm-pi-CO2pulse",
                 "esm-pi-cdr-pulse", "esm-piControl", "1pctCO2", "1pctCO2-4xext")

# 2. Convert to Hector Units  ---------------------------------------------------------------------
# Select the secnarios to prep as Hector input and add the unit conversion information.
cmip6_inputs <- raw_inputs[raw_inputs$Scenario %in% c(phase6_scns, DECK_scns), ]
cmip6_inputs_units <- dplyr::inner_join(cmip6_inputs,
                                        conversion_df, by = c('Variable' = 'rcmip_variable'))

# Convert from the rcmip units to the hector units.
converted_value <- udunits2::ud.convert(cmip6_inputs_units$value,
                                                 cmip6_inputs_units$rcmip_udunits,
                                                 cmip6_inputs_units$hector_udunits)
cols_to_keep <- c('Scenario', 'year')
converted_cmip6 <- cmip6_inputs_units[ , names(cmip6_inputs_units) %in% cols_to_keep]
names(converted_cmip6) <- tolower(cols_to_keep)
converted_cmip6$value <- converted_value
converted_cmip6$units <- cmip6_inputs_units$hector_unit
converted_cmip6$variable <- cmip6_inputs_units$hector_variable
converted_cmip6 <- remove_spaces(converted_cmip6, cols = c("units", "variable"))

# 3. Fill In Missing Years ------------------------------------------------------------------
# Extraploate the values for the missing years.
converted_cmip6 %>%
  # Some of the scenarios stop too early to use as Hector input, for these scenarios / variables
  # we will need to extrapolate the time series back to 1700.
  # Add missing years with NA values.
  tidyr::complete(tidyr::nesting(scenario, variable, units), year = c(year, 1700:2500)) %>%
  dplyr::arrange(scenario, variable, units, year) %>%
  dplyr::group_by(scenario, variable, units) %>%
  # Linerally interpolate for the missing values when the NA occurs within a time series of data.
  # And extraplate with the nearest neightboor if the NAs occur at the begging or end of a
  # time series
  dplyr::mutate(value = zoo::na.approx(value, na.rm = FALSE, rule = 2)) %>%
  dplyr::ungroup() ->
  cmip6_full

# 4. Seperate Into the Emissions and Concentration Constraints --------------------------------
# TODO why do we have both concentrations and constraints??
emissions_data    <- cmip6_full[grepl(pattern = 'emissions', x = cmip6_full$variable), ]
concenration_data <- cmip6_full[grepl(pattern = 'constrain|concentration', x = cmip6_full$variable), ]


# 4. Format and Save -----------------------------------------------------------------------
# A function to format the input time series into csv formats for Hector
# Args:
#   input: a dataframe of inputs for Hector
#   scns: a vector of the scenario names, to be used to select what data to process
#   paths: a vector of the names of the output csv files, the names should be in the same order
#         as the scns argument.
# Returns: writes the csv file to disk and can return the out put file name.
# TODO figure out a better way to do the scns/paths thing?
# Should we have a test? Do we need to check to see if the result can be input with other ini files?
format_save_csv_files <- function(input, scns, paths){

  files <- mapply(function(scn_name, out_file){

    data <- input[input$scenario == scn_name, names(input) %in%  c('variable', 'year', 'value')]
    assertthat::are_equal(sum(is.na(data)), 0)

    # Convert from a wide data frame to a long data frame with the correct column names
    # that match the inputs included for Hector.
    # See https://github.com/JGCRI/hector/tree/master/inst/input/emissions for examples.
    out <- tidyr::pivot_wider(data,  names_from = variable, values_from = value)
    names(out)[[1]] <- 'Date'

    write.csv(x = out, file = out_file, row.names = FALSE)

    out_file},
    scn_name = scns,
    out_file = paths)

  invisible(files)

}


# Concentration Constraints
# Define where the output csv files are going to go
conc_scns      <- unique(concenration_data$scenario)
conc_out_dir   <- here::here('inst', 'input', 'CMIP6', 'constraints'); dir.create(conc_out_dir, showWarnings = FALSE, recursive = TRUE)
conc_out_files <- file.path(conc_out_dir, paste0(conc_scns, '.csv'))

format_save_csv_files(input = concenration_data, scns = conc_scns, paths = conc_out_files)

# Emissions
ems_scns      <- unique(emissions_data$scenario)
ems_out_dir   <- here::here('inst', 'input', 'CMIP6', 'emissions'); dir.create(ems_out_dir, showWarnings = FALSE, recursive = TRUE)
ems_out_files <- file.path(ems_out_dir, paste0(ems_scns, '.csv'))

format_save_csv_files(input = emissions_data, scns = ems_scns, paths = ems_out_files)
