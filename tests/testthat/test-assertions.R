context("assertions")

test_that("get_assert_method correctly choses assertion method", {
  fun_1 <- function() 1
  fun_2 <- function() 2
  method_raw <- data.validator:::get_assert_method(
    function(x) c(TRUE, FALSE), list(direct = fun_1, generator = fun_2)
  )
  method_gen <- data.validator:::get_assert_method(
    function(x) function(y) c(TRUE, FALSE), list(direct = fun_1, generator = fun_2)
  )
  expect_equal(method_raw, fun_1)
  expect_equal(method_gen, fun_2)
  expect_error(
    data.validator:::get_assert_method(function(x) c(1, 2), list(direct = fun_1, generator = fun_2))
  )
})

test_that("Validation works even with evaluation error", {
  validation_result <-
    data.validator::validate(iris, name = "Iris wrong column test") %>%
    validate_if(is.character(Speciies))

  expect_equal(nrow(validation_result), nrow(iris))
  expect_length(attr(validation_result, "assertr_errors"), 1)
})

test_that("validate_cols selects all columns if there are no columns selected", {
  data <- data.frame(V1 = c("c", "d"), V2 = c(2, 2), V3 = c(1, 1))

  result <- validate(data) %>%
    validate_cols(is.character)

  expect_equal(length(attr(result, which = "assertr_errors")), 2)
})

test_that("validate_cols throws a message if there are no columns selected", {
  data <- data.frame(V1 = c("c", "d"), V2 = c(2, 2), V3 = c(1, 1))

  validation <- validate(data)

  expect_message(validate_cols(validation, function(x) TRUE))
})

test_that("validate_rows selects all columns if there are no columns selected", {
  data <- data.frame(
    V1 = c(1, 0, 0),
    V2 = c(0, 0, 0),
    V3 = c(0, 1, 0)
  )

  is_lower_than_one <- function(x) {
    x < 1
  }

  result <- validate(data) %>%
    validate_rows(rowSums, is_lower_than_one)

  expect_equal(nrow(attr(result, "assertr_errors")[[1]]$error_df), 2)
})

test_that("validate_rows throws a message if there are no columns selected", {
  data <- data.frame(
    V1 = c(1, 0, 0),
    V2 = c(0, 0, 0),
    V3 = c(0, 1, 0)
  )

  validation <- validate(data)

  expect_message(validate_rows(validation, rowSums, function(x) TRUE))
})
