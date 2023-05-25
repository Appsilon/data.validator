test_that("parse_errors_to_df parses correct assertion type and correct error_df", {
  data <- data.frame(
    V1 = c(1, 0),
    V2 = c(-1, -2)
  )
  validation <- validate_if(data, V1 < 0)
  parsed <- parse_errors_to_df(validation)

  expect_equal(parsed$type, error_id)
  expect_equal(nrow(parsed$error_df[[1]]), sum(!data$V1 < 0))
})

test_that("parse_successes_to_df parses success correctly", {
  data <- data.frame(
    V1 = c(1, 0),
    V2 = c(-1, -2)
  )
  data <- validate_if(data, V2 < 0)

  parsed <- parse_successes_to_df(data)

  expect_true(is.na(parsed$num.violations))
  expect_equal(parsed$type, success_id)
  expect_null(parsed$error_df[[1]])
})
