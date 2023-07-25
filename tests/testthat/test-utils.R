context("utils")

test_that("get_first_name obtains the name independently of how it was called", {
  browser()
  data_dummy <- data.frame(V1 = c(0,1,2), V2 = c(3,4,5))
  data <- data.frame(V1 = c(0,1,2), V2 = c(3,4,5))


  test_1 <- validate(data)
  test_2 <- data |> validate()
  test_4 <- data %>% validate()

  test_3 <- data |> dplyr::select(V1) |> validate()

  test_5 <- data %>% dplyr::select(V1) %>% validate()


  expect_equal(attr(test_1, "data-name"), "data")
  expect_equal(attr(test_2, "data-name"), "data")
  expect_equal(attr(test_3, "data-name"), "data") #
  expect_equal(attr(test_4, "data-name"), "data")
  expect_equal(attr(test_5, "data-name"), "data") #

  data_dummy |> dplyr::select(V1) |> dplyr::filter("V1" > 0) |> validate() |> attr("data-name")

  expect_equal(
    data_dummy |> validate() |> attr("data-name"),
    "data"
  )

  expect_equal(
    data_dummy %>% validate() %>% attr("data-name"),
    "data"
  )

  expect_equal(
    data_dummy %>% dplyr::select(V1) %>% validate() %>% attr("data-name"),
    "data"
  )

  data_dummy %>% dplyr::select(V1) %>% filter("V1" > 0) %>% validate() %>% attr("data-name")
  data_dummy |> dplyr::select(V1) |> filter("V1" > 0) |> validate() |> attr("data-name")

  data_dummy %>% dplyr::select(V1) |> validate() %>% attr("data-name")
  data_dummy |> dplyr::select(V1) %>% validate() |> attr("data-name")

  expect_equal(
    data_dummy |> dplyr::select(V1) |> validate() |> attr("data-name"),
    "data"
  )
})
