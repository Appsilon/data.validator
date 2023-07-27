library(testthat)
library(data.validator)

report_test <- function() {
  dat <- data.frame(
    V1 = c(1, 2, 3),
    V2 = c("a", "b", "c")
  )

  report <- data_validation_report()

  validate(dat, description = "A test report")  |>
    validate_cols(is.numeric, description = "Columns are numeric")  |>
    validate_if(.data$V1 > 0, description = "Is greater than 0")  |>
    add_results(report)

  return(report)
}

results_test <- function() {
  get_results(report_test())
}

test_check("data.validator", stop_on_warning = TRUE)
