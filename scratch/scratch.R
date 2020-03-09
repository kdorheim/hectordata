library(here)
source(here("R", "emissions.R"))

# === Setup test global vars ===================================================
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

# === Matrix from metadata & ffi columns test guts =============================
scenario <- "rcp60"
variable <- "ffi_emissions"
var_col  <- helper_var_col(scenario, variable)
ffi_col  <- var_col$v_col
rundates <- var_col$r_dates
meta_col <- get_meta_col(scenario, rundates)
output_matr <- lists_2_matrix(meta_col, ffi_col)
print(output_matr)
