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
