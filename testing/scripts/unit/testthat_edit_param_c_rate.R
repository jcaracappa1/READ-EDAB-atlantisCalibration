# File: test-edit_param_c_rate.R

library(testthat)

test_that("edit_param_c_rate behaves as expected and enforces validation", {
  
  # Ensure failure if file is missing
  expect_error(
    edit_param_c_rate(bio_prm = "missing_file.prm", new_c_rate = data.frame()), 
    "edit_param_c_rate: Biology parameter file not found."
  )
  
  valid_bio <- "testing/inputs/at_biology.prm"
  
  if (file.exists(valid_bio)) {
    # Ensure failure if overwrite = FALSE but no new filename is provided
    expect_error(
      edit_param_c_rate(bio_prm = valid_bio, new_c_rate = data.frame(), overwrite = FALSE),
      "edit_param_c_rate: new_file_name must be provided if overwrite is FALSE."
    )
    
    # Check cohort vector length validation
    too_long_vector <- rep(0.5, 50) # Assuming 50 exceeds max_age
    expect_error(
      edit_param_c_rate(bio_prm = valid_bio, new_c_rate = too_long_vector, overwrite = FALSE, new_file_name = "dummy.prm", single_group = TRUE, group_name = "BML"),
      "exceeds maximum age cohorts."
    )
  }
})