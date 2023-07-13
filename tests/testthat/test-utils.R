context("utils")

test_that("get_first_name obtains the name independently of how it was called", {
  data <- data.frame(V1 = c(0,1,2), V2 = c(3,4,5))

  test_1 <- validate(data)
  test_2 <- data |> validate()
  test_3 <- data |> dplyr::select(V1) |> validate()
  test_4 <- data %>% validate()
  test_5 <- data %>% dplyr::select(V1) %>% validate()

  browser()

  expect_equal(attr(test_1, "data-name"), "data")
  expect_equal(attr(test_2, "data-name"), "data")
  expect_equal(attr(test_3, "data-name"), "data")
  expect_equal(attr(test_4, "data-name"), "data")
  expect_equal(attr(test_5, "data-name"), "data")
})
