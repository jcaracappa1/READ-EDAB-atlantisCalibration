# compare_old_new_bh.R
# Missing Input Placeholders: None. Uses "at_biology.prm"

library(dplyr)

input_dir <- "testing/inputs"
out_dir <- "testing/outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Load Legacy functions
source("https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/refs/heads/dev_branch/R/Calibration_Tools/edit_param_BH.R") 
# Assuming both get and edit are in the same script online, otherwise adjust the legacy source accordingly.

# Load Refactored functions
source("R/get_param_bh.R")
source("R/edit_param_bh.R")

# 1. Compare GET function
old_get_df <- get_param_BH(file.path(input_dir, "at_biology.prm"))

# Original script reads all vals as characters. The new one explicitly coerces to numeric. 
# We coerce the old dataframe to numeric to test equivalence of the core data extraction.
old_get_df$alpha <- as.numeric(old_get_df$alpha)
old_get_df$beta <- as.numeric(old_get_df$beta)

new_get_df <- get_param_bh(file.path(input_dir, "at_biology.prm"))

print("Testing Get equivalence:")
print(all.equal(old_get_df, new_get_df))

# 2. Compare EDIT function
old_out_file <- file.path(out_dir, "at_biology_old.prm")
new_out_file <- file.path(out_dir, "at_biology_new.prm")

edit_param_BH(
  bio.prm = file.path(input_dir, "at_biology.prm"),
  group.name = "MAK",
  alpha = 3.7E9,
  beta = 7.56E13,
  overwrite = FALSE,
  new.file.name = old_out_file
)

edit_param_bh(
  bio_prm = file.path(input_dir, "at_biology.prm"),
  group_name ="MAK",
  alpha = 3.7E9,
  beta = 7.56E13,
  overwrite = FALSE,
  new_file_name = new_out_file
)

print("Testing Edit equivalence (Note: Old file uses space delimiters, New file uses Tab strictly per guide, so exact string matching might differ on spacing, but logical parameters will be equivalent):")
# You can check the extracted files again
print(all.equal(get_param_bh(old_out_file), get_param_bh(new_out_file)))
