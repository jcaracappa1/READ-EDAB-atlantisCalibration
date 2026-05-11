test_that("check_init_scale correctly processes log files", {
  # Create a mock log.txt content
  mock_log <- c(
    "timestep 0 biomass for HER initial 100.50 current 201.00",
    "timestep 0 biomass for COD initial 50.00 current 25.00"
  )
  
  tmp_dir <- tempdir()
  writeLines(mock_log, file.path(tmp_dir, "log.txt"))
  
  # The regex in tidyr::separate needs to match the mock format
  # Test successful execution
  result <- check_init_scale(param_dir = here::here('testing','inputs'), log_start = 1, n_groups = 89)
  
  expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$scalar[1], 2.0)
  
  # Test error handling for missing file
  expect_error(check_init_scale("non/existent/dir", 1, 2), "log.txt not found")
})
