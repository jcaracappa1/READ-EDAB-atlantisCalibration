#' Check initial scaling
#'
#' @title Check initial scaling of functional groups
#'
#' @description Generates a diagnostic data.frame and optional CSV to check that the realized initial conditions match those specified in the run.prm file.
#'
#' @param param_dir string. Path to parameter files.
#' @param out_dir string. Path to save output.
#' @param log_start numeric. Line number for start of first timestep in log.txt.
#' @param n_groups numeric. Number of functional groups in model.
#'
#' @return A data.frame containing functional group codes, initial biomass, first timestep biomass, and realized initial scalar.
#'
#' @export
check_init_scale <- function(param_dir, out_dir, log_start, n_groups) {
  
  log_path <- file.path(param_dir, "log.txt")
  
  # Validate file existence
  if (!file.exists(log_path)) {
    stop("check_init_scale: log.txt not found in param_dir.")
  }
  
  # Load in lines from log.txt
  con <- file(log_path, "r")
  log_lines <- readLines(con)
  close(con)
  
  # Validate log_start index
  if (log_start > length(log_lines)) {
    stop("check_init_scale: log_start exceeds the number of lines in log.txt.")
  }
  
  # Extract relevant lines for the functional groups
  log_subset <- log_lines[log_start:(log_start + n_groups - 1)]
  timesteps_df <- data.frame(line = log_subset, stringsAsFactors = FALSE)
  
  # Separate lines using tidyr; use specific names as per legacy logic
  line_split <- tidyr::separate(
    data = timesteps_df, 
    col = "line", 
    into = c("t", "z1", "z2", "z3", "s", "code", "a1", "a2", "i1", "i2", "b1", "b2", "b3", "b4", "tonnes_now1", "tonnes_now2", "c1", "c2", "c3", "tonnes_init1", "tonnes_init2", "d1"),
    sep = "[[:punct:][:space:]]+"
  )
  
  # Reconstruct numeric values and coerce
  tonnes_now <- as.numeric(paste0(line_split$tonnes_now1, ".", line_split$tonnes_now2))
  tonnes_init <- as.numeric(paste0(line_split$tonnes_init1, ".", line_split$tonnes_init2))
  
  # Create output data.frame
  vir_biomass_df <- data.frame(
    code = line_split$code, 
    biomass_now = tonnes_now, 
    biomass_init = tonnes_init,
    stringsAsFactors = FALSE
  )
  
  # Calculate scalar
  vir_biomass_df$scalar <- vir_biomass_df$biomass_now / vir_biomass_df$biomass_init
  
  # Write results if out_dir is provided
  if (!missing(out_dir)) {
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    utils::write.csv(
      vir_biomass_df, 
      file = file.path(out_dir, "Diagnostic_virgin_biomass_scalar.csv"), 
      row.names = TRUE
    )
  }
  
  return(vir_biomass_df)
}