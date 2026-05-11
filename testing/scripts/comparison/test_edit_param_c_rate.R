# File: compare_old_new.R

# ==============================================================================
# EQUIVALENCE TEST: compare_old_new.R
# ==============================================================================
# User Notice: This script relies on the 'at_biology.prm' file located within
# the 'testing/inputs' directory.
# ==============================================================================

library(dplyr)
library(utils)

# 1. Source the original legacy functions directly from the dev branch
source("https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/refs/heads/dev_branch/R/Calibration_Tools/edit_param_C_age.R")
# Note: The legacy file contains both get_ and edit_ so one source call is sufficient.

# 2. Source the newly refactored functions
source("R/get_param_c_rate.R")
source("R/edit_param_c_rate.R")

# 3. Setup test directories and files
input_bio <- "testing/inputs/at_biology.prm"
out_dir <- "testing/outputs/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- Test 1: get_param_C_age vs get_param_c_rate ---
old_get <- get_param_C_age(bio.prm = input_bio, write.output = FALSE) #Bug in old code for ages> 10
new_get <- get_param_c_rate(bio_prm = input_bio, write_output = FALSE)

# Compare logical equivalence
cat("Get Function Equivalence: ", isTRUE(all.equal(old_get, new_get, check.attributes = FALSE)), "\n")

# --- Test 2: edit_param_C_age vs edit_param_c_rate ---
# Setup mock data based on the extracted data.frame
mock_c_rate <- new_get[1:2, ] # Take first two groups
mock_c_rate[1, 2:11] <- rep(0.5, 10) 
mock_c_rate[2, 2:11] <- rep(0.6, 10)

old_out_file <- "testing/outputs/at_biology_old.prm"
new_out_file <- "testing/outputs/at_biology_new.prm"

edit_param_C_age(bio.prm = input_bio, new.C = mock_c_rate, overwrite = FALSE, new.file.name = old_out_file)
edit_param_c_rate(bio_prm = input_bio, new_c_rate = mock_c_rate, overwrite = FALSE, new_file_name = new_out_file)

# Compare edited files
old_lines <- readLines(old_out_file)
new_lines <- readLines(new_out_file)

cat("Edit Function Equivalence (File Match): ", identical(old_lines, new_lines), "\n")