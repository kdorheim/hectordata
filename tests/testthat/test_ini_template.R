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
  scenarios <- c("rcp45", "ssp245")
  hector_inputdir <- system.file('input', package='hector')
  # Create a new .ini file named "hector_rcp45_test.ini"
  test_scenario <- paste0("test_", scenario)
  create_scenario_ini(test_scenario)

  # Compare the new RCP45 .ini file to the default Hector RCP45 .ini file
  new_ini_name <- parse_ini_fname(test_scenario)
  new_ini_path <- file.path(inputdir, new_ini_name)
  new_ini      <- readLines( file(new_ini_path, open="w+") )

  ctrl_ini_name <- parse_ini_fname(scenario)
  ctrl_ini_path <- system.file(hector_inputdir, ctrl_ini_name)
  ctrl_ini      <- readLines( file(ctrl_ini_path, open="w+") )

  expect_true(all.equal(new_ini, ctrl_ini))
  # file.remove(new_ini_path)  # Remove hector_rcp45_test.ini (Raises Warning!)
})


