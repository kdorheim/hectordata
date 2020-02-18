context("Basic template .ini file I/O")

inputdir <- system.file('input', package='hectordata')

test_that("parse_emission_fname returns the proper emissions filename", {
  expect_equal(parse_emission_fname("rcp45"), "emissions/RCP45_emissions.csv")
  expect_equal(parse_emission_fname("rcp80"), "emissions/RCP80_emissions.csv")
  expect_equal(parse_emission_fname("ssp245"), "emissions/SSP245_emissions.csv")
  expect_equal(parse_emission_fname("ssp434"), "emissions/SSP434_emissions.csv")
  expect_equal(parse_emission_fname("esm-bell-1000"), "emissions/ESM-BELL-1000_emissions.csv")
})

test_that("parse_ini_fname returns the proper Hector input .ini filename", {
  expect_equal(parse_ini_fname("rcp45"), "hector_rcp45.ini")
  expect_equal(parse_ini_fname("rcp80"), "hector_rcp80.ini")
  expect_equal(parse_ini_fname("ssp245"), "hector_ssp245.ini")
  expect_equal(parse_ini_fname("ssp434"), "hector_ssp434.ini")
  expect_equal(parse_ini_fname("esm-bell-1000"), "hector_esm-bell-1000.ini")
})

test_that("Hector .ini file created by create_scenario_ini for RCP45", {
  scenario <- "rcp45"
  # Create a new .ini file named "hector_rcp45_test.ini"
  test_scenario <- paste0(scenario, "_test")
  create_scenario_ini(test_scenario)
  # Compare the new RCP45 .ini file to the default Hector RCP45 .ini file
  new_ini     <- system.file(inputdir, parse_ini_fname(test_scenario), package = "hectordata")
  control_ini <- system.file(inputdir, "hector_rcp45.ini", package = "hector")
  expect_true(all.equal(readLines(new_ini), readLines(control_ini)))
  file.remove(new_ini)  # Remove hector_rcp45_test.ini
})

test_that("Hector .ini file created by create_scenario_ini for SSP245", {
  scenario <- "ssp245"
  # Create a new .ini file named "hector_ssp245_test.ini"
  test_scenario <- paste0(scenario, "_test")
  create_scenario_ini(test_scenario)
  # Compare the new SSP245 .ini file to the default Hector SSP245 .ini file
  new_ini     <- system.file("input", parse_ini_fname(test_scenario), package = "hectordata")
  control_ini <- system.file("input", parse_ini_fname(scenario), package = "hector")
  expect_true(all.equal(readLines(new_ini), readLines(control_ini)))
  file.remove(new_ini)  # Remove hector_ssp245_test.ini
})
