context("Create Hector .ini files from template")

input_dir <- system.file('input', package='hectordata')
scenarios <- c("rcp45", "rcp85")

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

test_that("helper function that fetches .ini file from installed Hector package", {
  lapply(scenarios, function(scenario) {
    ini <- fetch_hector_ini(scenario)
    lbl <- paste0("Parsed default Hector ", scenario, " ini file having length > 0")
    expect_true(length(ini) > 0, label=lbl)
  })
})

test_that("Hector .ini file created by replace_ini_vars", {
  lapply(scenarios, function(scenario) {
    test_em_file <- parse_emission_fname(scenario)
    test_ini <- replace_ini_vars(scenario, test_em_file)
    ctrl_ini <- fetch_hector_ini(scenario)
    # --- Sanity check -----------------------------------------
    expect_true(length(test_ini) != 0)
    expect_true(length(ctrl_ini) != 0)
    expect_equal(length(test_ini), length(ctrl_ini))
    # --- Compare the lines containing emissions file names ----
    test_em_lines <- get_emission_lines(test_ini)
    ctrl_em_lines <- get_emission_lines(ctrl_ini)
    expect_true(all.equal(test_em_lines, ctrl_em_lines))
    # --- Compare the run names --------------------------------
    test_run_name <- get_run_name(test_ini)
    ctrl_run_name <- get_run_name(ctrl_ini)
    expect_true(all.equal(test_run_name, ctrl_run_name))
  })
})

test_that("create_scenario_ini raises an error when emissions file is not found", {
  lapply(scenarios, function(scenario) {
    em_fname <- parse_emission_fname(scenario)
    em_path <- file.path(input_dir, em_fname)
    err_msg <- paste0('Scenario emissions file not found: ', em_path)
    expect_error(create_scenario_ini(scenario), err_msg)
  })
})

