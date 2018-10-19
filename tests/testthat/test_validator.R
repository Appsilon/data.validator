context("Tests for Validator class")
library(dplyr)

test_that("Validator is empty after initialization", {
  validator <- Validator$new()
  results <- validator$get_validations()

  expect_equal(validator$repo_path, NULL)
  expect_equal(results$n_failed, 0)
  expect_equal(results$n_warned, 0)
  expect_equal(results$n_passed, 0)
  expect_equal(results$validation_results, tibble())
})

test_that("Validator adds validations correctly", {
  validator <- Validator$new()

  validator$add_validations(validated_data, "sample_file_path")
  results <- validator$get_validations()

  expect_equal(results$n_failed, n_failed)
  expect_equal(results$n_warned, n_warned)
  expect_equal(results$n_passed, n_passed)
  expect_equal(results$validation_results, validation_results)
})
