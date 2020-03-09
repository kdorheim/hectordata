# Test functions that create Hector-formatted RCMIP emissions files
#
# Matt Nicholson
# 5 Mar 2020

context("Create Hector-formatted RCMIP emissions files")

year_start <- 1745
year_end   <- 2100
rundates   <- seq(year_start, year_end)
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
  expect_equal(meta_col[[5]], as.character(year_start))
  expect_equal(meta_col[[length(meta_col)]], as.character(year_end))
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

test_that("Create matrix from metadata & ffi columns", {
  scenario <- "rcp60"
  variable <- "ffi_emissions"
  var_col  <- helper_var_col(scenario, variable)
  ffi_col  <- var_col$v_col
  rundates <- var_col$r_dates
  meta_col <- get_meta_col(scenario, rundates)
  output_matr <- lists_2_matrix(meta_col, ffi_col)
  expect_equal(class(output_matr), "matrix")
  expect_true(all(dim(output_matr) == c(340, 2)))
  # Add luc_emissions and re-check
  var_col <- helper_var_col(scenario, "luc_emissions")
  luc_col <- var_col$v_col
  output_matr <- add_list_2_matrix(output_matr, luc_col)
  expect_equal(class(output_matr), "matrix")
  expect_true(all(dim(output_matr) == c(340, 3)))
})

