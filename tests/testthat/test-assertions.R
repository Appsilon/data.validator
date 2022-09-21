context("validator")

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
