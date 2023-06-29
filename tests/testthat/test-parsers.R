context("parsers")

test_that("parse_errors_to_df parses correct error assertion type", {
  data <- data.frame(
    V1 = c(1, 0, -3)
  )
  validation <- validate(data) %>%
    validate_if(V1 < 0)

  # `error_id` is coming from `results_parsers.R`
  expect_equal(parse_errors_to_df(validation)$type, error_id)
})

test_that("parse_successes_to_df parses correct success assertion type", {
  data <- data.frame(
    V1 = c(0, 1, 0)
  )
  validation <- validate(data) %>%
    validate_rows(rowSums, assertr::in_set(0, 1), V1)

  # `success_id` is coming from `results_parsers.R`
  expect_equal(parse_successes_to_df(validation)$type, success_id)
})

test_that("parse_errors_to_df parses correct number of detected errors", {
  data <- data.frame(
    V1 = c(1, 0, -3)
  )
  validation <- validate_if(data, V1 < 0)
  parsed <- parse_errors_to_df(validation)

  expect_equal(nrow(parsed$error_df[[1]]), sum(!data$V1 < 0))
})
