# File: test-get_param_c_rate.R

library(testthat)

test_that("get_param_c_rate behaves as expected and handles errors", {
  
  # Ensure failure if file is missing
  expect_error(
    get_param_c_rate(bio_prm = "missing_file.prm"), 
    "get_param_c_rate: Biology parameter file not found."
  )
  
  # Ensure failure if attempting to write output without paths
  valid_bio <- "testing/inputs/at_biology.prm"
  if (file.exists(valid_bio)) {
    expect_error(
      get_param_c_rate(bio_prm = valid_bio, write_output = TRUE),
      "get_param_c_rate: output_dir and out_name must be provided if write_output is TRUE."
    )
    
    # Check successful extraction
    df_out <- get_param_c_rate(bio_prm = valid_bio, write_output = FALSE)
    expect_s3_class(df_out, "data.frame")
    expect_true("group" %in% colnames(df_out))
    expect_true(is.numeric(df_out[1,2])) # Ensure explicit numeric coercion occurred
  }
})