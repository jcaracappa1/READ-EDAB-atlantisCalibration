# Dependencies
library(tidyr)
library(utils)

# Load legacy function
source("https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/refs/heads/dev_branch/R/Calibration_Tools/check_init_scale.R")

# Load refactored function
source("R/check_init_scale.R")

# Setup paths
input_dir <- "testing/inputs"
output_dir <- "testing/outputs"

# Variables for test (Example values)
# PLACEHOLDER: log_start needs to be verified against the actual log.txt provided
test_log_start <- 100 
test_n_groups <- 89

# Run legacy
# Note: legacy used global 'atl.dir' inside function - setting it here
atl.dir <<- paste0(input_dir, "/") 
out_legacy <- check_init_scale(input_dir, paste0(output_dir, "/"), test_log_start, test_n_groups)

# Run refactored
out_new <- check_init_scale(input_dir, output_dir, test_log_start, test_n_groups)

# Comparison
res <- all.equal(out_legacy, out_new, check.attributes = FALSE)
if (isTRUE(res)) {
  message("Success: Legacy and Refactored outputs are equivalent.")
} else {
  stop("Failure: Outputs differ.")
}
