context("validator")

test_that("Validator is empty after initialization", {
  validator <- create_validator()
  results <- get_results(validator)

  expect_equal(private(validator, "n_failed"), 0)
  expect_equal(private(validator, "n_warned"), 0)
  expect_equal(private(validator, "n_passed"), 0)
  expect_equal(results, dplyr::tibble())
})

test_that("Validator adds validations correctly when there are warnings, errors and passed validations", {
  validator <- create_validator()

  add_results(validated_data, validator, "sample_data")
  results <- get_results(validator)

  expect_equal(private(validator, "n_failed"), 1)
  expect_equal(private(validator, "n_warned"), 1)
  expect_equal(private(validator, "n_passed"), 2)
  expect_equal(nrow(results), 4)
  expect_equal(results$type, c("error", "warning", "success", "success"))
})

test_that("Validator adds validations correctly when there are only warnings and passed validations", {
  validator <- create_validator()

  add_results(validated_data_no_errors, validator, "sample_data")
  results <- get_results(validator)

  expect_equal(private(validator, "n_failed"), 0)
  expect_equal(private(validator, "n_warned"), 1)
  expect_equal(private(validator, "n_passed"), 2)
  expect_equal(nrow(results), 3)
  expect_equal(results$type, c("warning", "success", "success"))
})

test_that("Validator adds validations correctly when there are only warnings", {
  validator <- create_validator()

  add_results(validated_data_only_warnings, validator, "sample_data")
  results <- get_results(validator)

  expect_equal(private(validator, "n_failed"), 0)
  expect_equal(private(validator, "n_warned"), 1)
  expect_equal(private(validator, "n_passed"), 0)
  expect_equal(nrow(results), 1)
  expect_equal(results$type, c("warning"))
})

test_that("Validator adds validations correctly when there are only passed validations", {
  validator <- create_validator()

  add_results(validated_data_only_passed, validator, "sample_data")
  results <- get_results(validator)

  expect_equal(private(validator, "n_failed"), 0)
  expect_equal(private(validator, "n_warned"), 0)
  expect_equal(private(validator, "n_passed"), 2)
  expect_equal(nrow(results), 2)
  expect_equal(results$type, c("success", "success"))
})

test_that("Validator adds validations correctly when there are no passed validations", {
  validator <- create_validator()

  add_results(validated_data_no_passed, validator, "sample_data")
  results <- get_results(validator)

  expect_equal(private(validator, "n_failed"), 1)
  expect_equal(private(validator, "n_warned"), 1)
  expect_equal(private(validator, "n_passed"), 0)
  expect_equal(nrow(results), 2)
  expect_equal(results$type, c("error", "warning"))
})
