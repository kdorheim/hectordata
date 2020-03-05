# Test functions that create Hector-formatted RCMIP emissions files
#
# Matt Nicholson
# 5 Mar 2020

context("Create Hector-formatted RCMIP emissions files")

test_that("Read and return the RCMIP emissions input file", {
  expected_cols <- c("Model", "Scenario", "Region", "Variable", "Unit",
                     "Activity_Id", "Mip_Era", "year", "value")
  rcmip_input <- get_rcmip_inputs()
  expect_true(identical(expected_cols, names(rcmip_input)))
})

test_that("Read and return the RCMIP to Hector variable Look-Up Table", {
  expected_cols <- c("hector_component", "hector_variable", "hector_unit",
                     "hector_udunits",   "rcmip_variable",  "rcmip_units",
                     "rcmip_udunits")
  lut <- rcmip2hector_df()
  expect_true(identical(expected_cols, names(lut)))
})

test_that("get_meta_col() produces correct metadata column", {
  year_start <- 1745
  year_end   <- 2100
  rundates   <- seq(year_start, year_end)
  scenario   <- "test_emissions"
  meta_col   <- get_meta_col(scenario, rundates)
  expect_equal(meta_col[[1]], paste0("; ", scenario, " emissions"))
  expect_equal(meta_col[[2]], "; Produced by Hectordata")
  expect_equal(meta_col[[3]], ";UNITS:")
  expect_equal(meta_col[[4]], "Date")
  expect_equal(meta_col[[5]], year_start)
  expect_equal(meta_col[[length(meta_col)]], year_end)
})
