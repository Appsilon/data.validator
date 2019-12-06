context("validator")

test_that("Validator is empty after initialization", {
  validator <- Validator$new()
  results <- validator$get_validations()

  expect_equal(validator$repo_path, NULL)
  expect_equal(results$n_failed, 0)
  expect_equal(results$n_warned, 0)
  expect_equal(results$n_passed, 0)
  expect_equal(results$validation_results, dplyr::tibble())
})

test_that("Validator adds validations correctly when there are warnings, errors and passed validations", {
  validator <- Validator$new()

  validator$add_validations(validated_data, "sample_file_path")
  results <- validator$get_validations()

  expect_equal(results$n_failed, n_failed)
  expect_equal(results$n_warned, n_warned)
  expect_equal(results$n_passed, n_passed)
  expect_equal(results$validation_results, validation_results)
})

test_that("Validator adds validations correctly when there are only warnings or errors and passed validations", {
  validator <- Validator$new()

  validator$add_validations(validated_data_no_errors, "sample_file_path")
  results <- validator$get_validations()

  expect_equal(results$n_failed, n_failed_no_errors)
  expect_equal(results$n_warned, n_warned_no_errors)
  expect_equal(results$n_passed, n_passed_no_errors)
  expect_equal(results$validation_results, validation_results_no_errors)
})

test_that("Validator adds validations correctly when there are only passed validations", {
  validator <- Validator$new()

  validator$add_validations(validated_data_only_passed, "sample_file_path")
  results <- validator$get_validations()

  expect_equal(results$n_failed, n_failed_only_passed)
  expect_equal(results$n_warned, n_warned_only_passed)
  expect_equal(results$n_passed, n_passed_only_passed)
  expect_equal(results$validation_results, validation_results_only_passed)
})

test_that("Validator adds validations correctly when there are no passed validations", {
  validator <- Validator$new()

  validator$add_validations(validated_data_no_passed, "sample_file_path")
  results <- validator$get_validations()

  expect_equal(results$n_failed, n_failed_no_passed)
  expect_equal(results$n_warned, n_warned_no_passed)
  expect_equal(results$n_passed, n_passed_no_passed)
  expect_equal(results$validation_results, validation_results_no_passed)
})
