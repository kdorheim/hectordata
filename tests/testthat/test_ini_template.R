context("Basic template .ini file I/O")

test_that("parse_emission_fname returns the proper emissions filename", {
  expect_equal(parse_emission_fname("rcp45"), "emissions/RCP45_emissions.csv")
  expect_equal(parse_emission_fname("rcp80"), "emissions/RCP80_emissions.csv")
  expect_equal(parse_emission_fname("ssp245"), "emissions/SSP245_emissions.csv")
  expect_equal(parse_emission_fname("ssp434"), "emissions/SSP434_emissions.csv")
  expect_equal(parse_emission_fname("esm-bell-1000"), "emissions/ESM-BELL-1000_emissions.csv")
})
