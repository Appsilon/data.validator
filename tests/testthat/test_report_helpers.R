context("Tests for report_helpers.R")

test_that("generate_id creates valid character", {
  expect_true(is.character(generate_id()))
})
