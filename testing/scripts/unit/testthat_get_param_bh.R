# test-get_param_bh.R

library(testthat)
library(dplyr)

test_that("get_param_bh handles invalid paths", {
  expect_error(get_param_bh("testing/inputs/nonexistent_bio.prm"), "not found")
})

test_that("get_param_bh successfully reads and coerces types", {
  # Requires at_biology.prm to be present
  bh_df <- get_param_bh("testing/inputs/at_biology.prm")
  
  expect_s3_class(bh_df, "data.frame")
  expect_true(is.numeric(bh_df$alpha))
  expect_true(is.numeric(bh_df$beta))
  expect_true(is.character(bh_df$group) || is.factor(bh_df$group))
  expect_true(nrow(bh_df) > 0)
})