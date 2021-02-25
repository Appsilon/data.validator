context("validator")

test_that("get_assert_method correctly choses assertion method", {
  fun_1 <- function() 1
  fun_2 <- function() 2
  method_raw <- get_assert_method(function(x) c(TRUE, FALSE), list(direct = fun_1, generator = fun_2))
  method_gen <- get_assert_method(function(x) function(y) c(TRUE, FALSE), list(direct = fun_1, generator = fun_2))
  expect_equal(method_raw, fun_1)
  expect_equal(method_gen, fun_2)
  expect_error(get_assert_method(function(x) c(1, 2), list(direct = fun_1, generator = fun_2)))
})
