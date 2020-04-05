#  Convert the CMIP6 inputs provided by the RCMIP activities to the appropriate units
# for Hector to use.

# TODO:
# 1. Do we want to include a CMIP5 scenario as a test to make sure that the unit
#    conversions are happening correctly? In the first submission to RCMIP this was and issue.
# 2. What are we going to do about the fact that the .fst file comes from the hector-rcmip package?
# 3. What do we want to make the naming and structure consistent with the Hector files or with
#    the CMIP files?
# 4. What do we want to do about the upper case vs lower case situation?
# 5. Figure out how to create subdirectories in the input folder, to allow for the different MIP eras
# Other are several other function specific TODOs that may help with the enhancement
# of this script.
# 6. What do we want to do about the ud2 convert units functions / testing ?
# 7. Clean up the documentation.


# 0. Set Up ---------------------------------------------------------------------
# Import required libs
library(magrittr)

# Define the input directory
INPUT_DIR <- here::here('data-raw', 'CMIP6')


# Define helpful functions
#
# Remove spaces from a column of a data frame
# Inputs
#   df: a tibble or data frame
#   cols: a vector of columns in the data frame to remove a space from the beginning of a string
# Returns: a data frame of the same dimensions but the modified columns have removed the " " that
# starts some of the character strings.
remove_spaces <- function(df, cols){

  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(all(cols %in% names(df)))

  lapply(cols, function(col){

    assertthat::assert_that(is.character(df[[col]]))
    df[[col]] <<- gsub(pattern = '^ ', replacement = '', x = df[[col]])

  })

  df

}


# 1. Import Data  ---------------------------------------------------------------------
# Import the rcmip inputs, note that this file might be updated on https://www.rcmip.org/,
# was most likely created by https://github.com/ashiklom/hector-rcmip and that the RCMIP
# community might update the files, and if that is the case the rcmip-inputs.fst will
# also need to be updated.
# TODO should this be modified so that it pulls the csv files from the website?
raw_inputs <- tibble::as_tibble(fst::read_fst(file.path(INPUT_DIR, 'rcmip-inputs.fst')))
raw_inputs <- remove_spaces(raw_inputs, cols = c("Variable"))

# Read in the csv file that will be used to convert from the rcmip units to the hector units.
conversion_df <- read.csv(file.path(INPUT_DIR, "variable-conversion.csv"), stringsAsFactors = FALSE)
conversion_df <- remove_spaces(conversion_df, cols = c("hector_variable", "rcmip_variable",
                                                       "rcmip_udunits", "hector_udunits"))

# The CMIP6 scenarios consist of the phase specific and DECK scenarios.
# TODO for right now the rcmip rcp45 scenario is included to make sure that inputs
# are being converted properly because it can so easily be compared with inputs included
# in the hector package.
phase6_scns <- c("ssp370", "ssp370-lowNTCF", "ssp434", "ssp460",
                 "ssp119", "ssp126", "ssp245", "ssp534-over",
                 "ssp585")
DECK_scns   <- c("abrupt-0p5xCO2", "abrupt-2xCO2", "abrupt-4xCO2",
                 "historical", "piControl", "esm-bell-1000PgC",
                 "esm-bell-2000PgC", "esm-bell-750PgC", "esm-pi-CO2pulse",
                 "esm-pi-cdr-pulse", "esm-piControl", "1pctCO2", "1pctCO2-4xext")
cmip5_test <- c("rcp45")

# 2. Convert to Hector Units  ---------------------------------------------------------------------
# Select the scenarios to prep as Hector input and add the unit conversion information.
cmip6_inputs <- raw_inputs[raw_inputs$Scenario %in% c(phase6_scns, DECK_scns, cmip5_test), ]
cmip6_inputs_units <- dplyr::inner_join(cmip6_inputs,
                                        conversion_df, by = c('Variable' = 'rcmip_variable'))

# TODO need to check in with AS to see if we can borrow the unit conversion code and also
# need to add some sort of exportable function and test it some.
# Do we need to do a suite of unit conversion tests?
# Also is there a way to optimize this? Right now it is slow.
mapply(ud_convert2,
       x = cmip6_inputs_units$value,
       from = cmip6_inputs_units$rcmip_udunits,
       to = cmip6_inputs_units$hector_udunits,
       SIMPLIFY = FALSE) %>%
  unlist ->
new_values

cmip6_inputs_units %>%
  dplyr::mutate(value = new_values) %>%
  dplyr::select(scenario = Scenario,
                year,
                value,
                variable = hector_variable,
                units = hector_unit) ->
    converted_cmip6

# 3. Fill In Missing Years ------------------------------------------------------------------
# Extrapolate the values for the missing years.
converted_cmip6 %>%
  # Some of the scenarios stop too early to use as Hector input, for these scenarios / variables
  # we will need to extrapolate the time series back to 1700.
  # Add missing years with NA values.
  tidyr::complete(tidyr::nesting(scenario, variable, units), year = c(year, 1700:2500)) %>%
  dplyr::arrange(scenario, variable, units, year) %>%
  dplyr::group_by(scenario, variable, units) %>%
  # Linearly interpolate for the missing values when the NA occurs within a time series of data.
  # And extrapolate with the nearest neighbor if the NAs occur at the begging or end of a
  # time series.
  dplyr::mutate(value = zoo::na.approx(value, na.rm = FALSE, rule = 2)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year %in% 1745:2500) ->
  cmip6_full

# 4. Format into Complete Hector Tables -----------------------------------------------------------------------------
# Format the input data into the Hector appropriate emissions or concentration input csv file.
#
# Args:
#   input: a data frame of hector inputs
#   ems: a Boolean to indicate if the emissions or concentration input is being formatted
# TODO this function is not great and there are some issues with the assumption of things
# when they are missing or extra which may need to be corrected.

foramt_hector_table <- function(input, ems = TRUE){

  assertthat::assert_that(is.data.frame(input))
  assertthat::are_equal(sum(is.na(input)), 0)
  assertthat::assert_that(length(unique(input$scenario)) == 1, msg = 'There is an issue with the number of scenarios being read into this function')

  # TODO need to add something that will check to see if these
  # columns of the data frame exist or not.
  input <- input[ , names(input) %in% c('year', 'variable', 'value')]
  input <- input[input$variable %in% c(hector_constraints, hector_emissions), ]
  out <- tidyr::pivot_wider(input, names_from = variable, values_from = value)
  names(out)[[1]] <- 'Date'

  if(ems){
    missing <- hector_emissions[which(!hector_emissions %in% names(out))]

  } else {
    missing <- hector_constraints[which(!hector_constraints %in% names(out))]
  }

  if(length(missing) > 0){
    # TODO If there is missing data fill it with 0, this assumption will
    # probably change latter.
    new_data <- matrix(data = 0, nrow = nrow(out), ncol = length(missing))
    colnames(new_data) <- missing
    out <- cbind(out, new_data)
  }

  out %>%
  replace(is.na(.), 0)

}

cmip6_full %>%
  dplyr::filter(grepl(pattern = 'emissions', x = variable)) %>%
  split(.$scenario) %>%
  lapply(foramt_hector_table) ->
  emissions_tables

cmip6_full %>%
  dplyr::filter(grepl(pattern = 'constrain', x = variable)) %>%
  split(.$scenario) %>%
  lapply(foramt_hector_table) ->
  concentration_table

# 5. Save Output --------------------------------------------------------------------------
# Format and save output
# Args
#   x: a dataframe object containing Hector input timeseries to be written out
#   path: the path name of the csv file to object x at
# Return: a csv file formatted with a header that can be read into Hector
save_output <- function(x, path){
  # TODO there has to be a better way to do this but for now this is the hack we will use,
  # write the csv file out and read it in as character lines, this will allow us to
  # add the header information.
  # This function should probably be exported by the package
  readr::write_csv(x, path, append = FALSE, col_names = TRUE)
  lines <- readLines(path)
  # TODO add the capability to add unit information and the actual scneario name.
  new_lines <- append(c('; scenario name',
                      '; created by hectordata',
                      '; units'),
                      lines)
  writeLines(new_lines, path)
}

# Hector emission inputs
ems_out_dir    <- here::here('inst', 'input', 'emissions'); dir.create(ems_out_dir, showWarnings = FALSE, recursive = TRUE)
ems_out_files  <- file.path(ems_out_dir, paste0(toupper(names(emissions_tables)), '_emissions.csv'))
files <- mapply(save_output, x = emissions_tables, path = ems_out_files)


# Hector concentration inputs
conc_out_dir    <- here::here('inst', 'input', 'constraints'); dir.create(conc_out_dir, showWarnings = FALSE, recursive = TRUE)
conc_out_files  <- file.path(conc_out_dir, paste0(toupper(names(concentration_table)), '_constraints.csv'))
files <- mapply(save_output, x = concentration_table, path = conc_out_files)


