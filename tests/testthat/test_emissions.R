# Test functions that create Hector-formatted RCMIP emissions files
#
# Matt Nicholson
# 5 Mar 2020

context("Create Hector-formatted RCMIP emissions files")

year_start <- 1745
year_end   <- 2100

run_start  <- 1765
run_end    <- 2100
rundates   <- seq(run_start, run_end)

rcmip_inputs <- get_rcmip_inputs()
rcmip2hector_lut <- rcmip2hector_df()

# === Helper functions =========================================================
helper_var_col <- function(scenario, var_name) {
  input_sub <- filter_rcmip_inputs(rcmip_inputs, scenario, year_start, year_end)
  minyear   <- max(min(input_sub$year), year_start)
  maxyear   <- min(max(input_sub$year), year_end)
  rundates  <- seq(minyear, maxyear)
  var_sub   <- subset_hector_var(input_sub, rcmip2hector_lut, var_name)
  var_col   <- get_variable_col(var_sub, rcmip2hector_lut, rundates)
  invisible(list(v_col=var_col, r_dates=rundates))
}

get_matr_vars <- function() {
  vars <- c("Date", "ffi_emissions", "luc_emissions", "CH4_emissions",
            "NOX_emissions", "CO_emissions", "NMVOC_emissions", "N2O_emissions",
            "Ftalbedo", "SO2_emissions", "SV", "BC_emissions", "OC_emissions",
            "C2F6_emissions", "CCl4_emissions", "CF4_emissions", "CFC11_emissions",
            "CFC113_emissions", "CFC114_emissions", "CFC115_emissions","CFC12_emissions",
            "CH3Br_emissions", "CH3CCl3_emissions", "CH3Cl_emissions", "HFC125_emissions",
            "HFC134a_emissions", "HFC143a_emissions", "HFC227ea_emissions", "HFC23_emissions",
            "HFC32_emissions", "SF6_emissions")
  return(vars)
}

get_matr_units <- function() {
  units <- c(";UNITS:", "PgC year-1", "PgC year-1", "Tg CH4 year-1", "Tg N year-1",
             "Tg CO year-1", "Tg NMVOC year-1", "Tg N year-1", "W m-2", "Gg S year-1",
             "W m-2", "Tg year-1", "Tg year-1", "ppt", "ppt", "ppt", "ppt", "ppt",
             "ppt", "ppt", "ppt", "ppt", "ppt", "ppt", "ppt", "ppt", "ppt", "ppt",
             "ppt", "ppt", "ppt")
  return(units)
}

# === Test functions ===========================================================
test_that("Read and return the RCMIP emissions input file", {
  expected_cols <- c("Model", "Scenario", "Region", "Variable", "Unit",
                     "Activity_Id", "Mip_Era", "year", "value")
  expect_true(identical(expected_cols, names(rcmip_inputs)))
})

test_that("Read and return the RCMIP to Hector variable Look-Up Table", {
  expected_cols <- c("hector_component", "hector_variable", "hector_unit",
                     "hector_udunits",   "rcmip_variable",  "rcmip_units",
                     "rcmip_udunits")
  expect_true(identical(expected_cols, names(rcmip2hector_lut)))
})

test_that("get_meta_col() produces correct metadata column", {
  scenario   <- "test_emissions"
  meta_col   <- get_meta_col(scenario, rundates)
  expect_equal(meta_col[[1]], paste0("; ", scenario, " emissions"))
  expect_equal(meta_col[[2]], "; Produced by Hectordata R package")
  expect_equal(meta_col[[3]], ";UNITS:")
  expect_equal(meta_col[[4]], "Date")
  expect_equal(meta_col[[5]], as.character(run_start))
  expect_equal(meta_col[[length(meta_col)]], as.character(run_end))
})

test_that("Construct output emissions dataframe column with get_variable_col()", {
  scenario <- "rcp60"
  variable <- "ffi_emissions"
  ffi_ret_val <- helper_var_col(scenario, variable)
  ffi_col  <- ffi_ret_val$v_col
  expect_equal(ffi_col[[1]], "")
  expect_equal(ffi_col[[2]], "")
  expect_equal(ffi_col[[3]], "PgC year-1")
  expect_equal(ffi_col[[4]], "ffi_emissions")
})

test_that("Metadata column and FFI column have same length", {
  scenario <- "rcp60"
  variable <- "ffi_emissions"
  var_col  <- helper_var_col(scenario, variable)
  ffi_col  <- var_col$v_col
  rundates <- var_col$r_dates
  meta_col <- get_meta_col(scenario, rundates)
  expect_equal(length(ffi_col), length(meta_col))
})

test_that("Create matrix from metadata, ffi, & luc columns", {
  scenario <- "rcp60"
  variable <- "ffi_emissions"
  var_col  <- helper_var_col(scenario, variable)
  ffi_col  <- var_col$v_col
  rundates <- var_col$r_dates
  meta_col <- get_meta_col(scenario, rundates)
  output_matr <- lists_2_matrix(meta_col, ffi_col)
  expect_equal(class(output_matr), "matrix")
  expect_true(all(dim(output_matr) == c(340, 2)))
  # --- Add luc_emissions column and re-check ---
  var_col <- helper_var_col(scenario, "luc_emissions")
  luc_col <- var_col$v_col
  output_matr <- add_list_2_matrix(output_matr, luc_col)
  expect_equal(class(output_matr), "matrix")
  expect_equal(output_matr[[5, 1]], as.character(rundates[1]))     # First year
  expect_equal(output_matr[[340, 1]], as.character(rundates[336])) # Last year
  expect_true(all(dim(output_matr) == c(340, 3)))
  expect_true(all(output_matr[1,] == c("; rcp60 emissions", "", "")))
  expect_true(all(output_matr[2,] == c("; Produced by Hectordata R package", "", "")))
  expect_true(all(output_matr[3,] == c(";UNITS:", "PgC year-1", "PgC year-1")))
  expect_true(all(output_matr[4,] == c("Date", "ffi_emissions", "luc_emissions")))
})

test_that("Create Hector input emissions file via generate_emissions()", {
  var_names <- get_matr_vars()
  var_units <- get_matr_units()
  num_vars = length(var_names) - 1
  scenario <- "rcp60"
  outpath  <- file.path("inst", "input", "emissions")
  debug    <- TRUE
  output_matr <- generate_emissions(scenario, outpath, debug)
  expect_equal(class(output_matr), "matrix")
  expect_true(all(output_matr[1,] == c("; rcp60 emissions", rep(c(""), num_vars))))
  expect_true(all(output_matr[2,] == c("; Produced by Hectordata R package", rep(c(""), num_vars))))
  expect_true(all(output_matr[4,] == var_names))
  expect_true(all(output_matr[3,] == var_units))
  expect_true(all(dim(output_matr) == c(340, length(var_names))))
  expect_equal(output_matr[[5, 1]], as.character(run_start))
  expect_equal(output_matr[[340, 1]], as.character(run_end))
})
