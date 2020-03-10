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
# scenario <- "rcp60"
# variable <- "ffi_emissions"
# var_col  <- helper_var_col(scenario, variable)
# ffi_col  <- var_col$v_col
# rundates <- var_col$r_dates
# meta_col <- get_meta_col(scenario, rundates)
# output_matr <- lists_2_matrix(meta_col, ffi_col)
# var_col <- helper_var_col(scenario, "luc_emissions")
# luc_col <- var_col$v_col
# output_matr <- add_list_2_matrix(output_matr, luc_col)
# colnames(output_matr) <- NULL
# print(output_matr)

# === Ellipses argument passing to subfunction =================================
func_a <- function(scenario, year_first, year_last) {
  print('In func_a')
  scenario <- toupper(scenario)
  scenario <- gsub("_", "", scenario)
  f_name <- paste0(scenario, "_emissions.csv")
  func_b(scenario, year_first, year_last, em_fname=f_name)#, interpolate=T)
}

func_b <- function(scenario, year_first, year_last, ...) {
  print('In func_b')
  args <- list(...)
  if (is.null((args$interpolate))) {args$interpolate = TRUE}
  print(args$em_fname)
  func_c(scenario, args$em_fname, args$interpolate)
}

func_c <- function(scenario, em_fname, interpolate=TRUE) {
  print('In func_c')
  print(paste0('Emissions filename: ', em_fname))
  if (interpolate) {
    print('Interpolating!')
  }
}

func_a(scenario, year_start, year_end)

