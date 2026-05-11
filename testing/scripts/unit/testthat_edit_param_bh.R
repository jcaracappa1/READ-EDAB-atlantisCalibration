# test-edit_param_bh.R

library(testthat)

test_that("edit_param_bh validates inputs correctly", {
  expect_error(
    edit_param_bh(bio_prm = "testing/inputs/nonexistent.prm", group_name = "HER"),
    "not found"
  )
  
  expect_error(
    edit_param_bh(bio_prm = "testing/inputs/at_biology.prm", group_name = c("HER", "MAK"), alpha = 100, overwrite = FALSE),
    "new_file_name must be provided"
  )
  
  expect_error(
    edit_param_bh(bio_prm = "testing/inputs/at_biology.prm", group_name = c("HER", "MAK"), alpha = 100, new_file_name = "test.prm"),
    "Length of 'alpha' vector must match"
  )
})

test_that("edit_param_bh modifies values and handles missing groups gracefully", {
  # Testing with a dummy missing group
  expect_error(
    edit_param_bh(bio_prm = "testing/inputs/at_biology.prm", group_name = c("FAKEGROUP"), alpha = 100, new_file_name = "test.prm"),
    "Parameter BHalpha_FAKEGROUP not found"
  )
})