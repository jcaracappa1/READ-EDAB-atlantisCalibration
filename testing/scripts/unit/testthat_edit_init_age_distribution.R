# test-edit_init_age_distribution.R
library(testthat)
library(dplyr)
library(ncdf4)

test_that("edit_init_age_distribution validates input parameters appropriately", {
  # Should fail when initialization file does not exist
  expect_error(
    edit_init_age_distribution(
      bio_prm = "dummy_bio.prm",
      fgs_file = "dummy_fgs.csv",
      ss_conv_file = "dummy.csv",
      ss_cat_file = "dummy.csv",
      box_prop_file = "dummy.csv",
      peak_age = 3,
      steepness = 1.5,
      ref_run_dir = "dummy_dir",
      init_size_age = "dummy.csv",
      ss_adj_abund_file = "dummy.csv",
      init_file = "nonexistent_file.nc",
      overwrite = FALSE,
      new_init_file = "new_file.nc"
    ),
    "not found"
  )
  
  # Should fail when new_init_file is omitted but overwrite is FALSE
  expect_error(
    edit_init_age_distribution(
      bio_prm = "dummy_bio.prm",
      fgs_file = "dummy_fgs.csv",
      ss_conv_file = "dummy.csv",
      ss_cat_file = "dummy.csv",
      box_prop_file = "dummy.csv",
      peak_age = 3,
      steepness = 1.5,
      ref_run_dir = "dummy_dir",
      init_size_age = "dummy.csv",
      ss_adj_abund_file = "dummy.csv",
      init_file = "testing/inputs/neus_output.nc", # Assuming this exists locally during testing
      overwrite = FALSE,
      new_init_file = NULL
    ),
    "new_init_file must be provided if overwrite is FALSE"
  )
})