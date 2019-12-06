validated_data <- dplyr::tibble()
attr(validated_data, "assertr_errors") <- list(
  list(
    message = "This is error",
    num.violations = 1,
    validation_id = 1))
attr(validated_data, "assertr_warnings") <- list(
  list(
    message = "This is warning",
    num.violations = 1,
    validation_id = 2))
attr(validated_data, "assertr_results") <- list(
  data.frame(
    title = "Error",
    result = FALSE,
    validation_id = 1,
    stringsAsFactors = FALSE),
  data.frame(
    title = "Warning",
    result = FALSE,
    validation_id = 2,
    stringsAsFactors = FALSE),
  data.frame(
    title = "Ok",
    result = TRUE,
    validation_id = 3,
    stringsAsFactors = FALSE))

n_failed <- n_passed <- n_warned <- 1

validation_results <- dplyr::tibble(
  title = c("Error", "Warning", "Ok"),
  result = c("Failed", "Warning", "Passed"),
  validation_id = as.numeric(1:3),
  object = rep("validated_data", 3),
  file_path = rep("sample_file_path", 3),
  message = c("This is error", "This is warning", NA),
  num.violations = c(1, 1, NA)
)

validated_data_no_errors <- dplyr::tibble()
attr(validated_data_no_errors, "assertr_warnings") <- list(
  list(
    message = "This is warning",
    num.violations = 1,
    validation_id = 2))
attr(validated_data_no_errors, "assertr_results") <- list(
  data.frame(
    title = "Warning",
    result = FALSE,
    validation_id = 2,
    stringsAsFactors = FALSE),
  data.frame(
    title = "Ok",
    result = TRUE,
    validation_id = 3,
    stringsAsFactors = FALSE))

n_failed_no_errors <- 0
n_passed_no_errors <- n_warned_no_errors <- 1

validation_results_no_errors <- dplyr::tibble(
  title = c("Warning", "Ok"),
  result = c("Warning", "Passed"),
  validation_id = as.numeric(2:3),
  object = rep("validated_data_no_errors", 2),
  file_path = rep("sample_file_path", 2),
  message = c("This is warning", NA),
  num.violations = c(1, NA)
)

validated_data_only_passed <- dplyr::tibble()
attr(validated_data_only_passed, "assertr_results") <- list(
  data.frame(
    title = "Ok",
    result = TRUE,
    validation_id = 3,
    stringsAsFactors = FALSE))

n_failed_only_passed <- n_warned_only_passed <- 0
n_passed_only_passed <- 1

validation_results_only_passed <- dplyr::tibble(
  title = "Ok",
  result = "Passed",
  validation_id = 3,
  object = "validated_data_only_passed",
  file_path = "sample_file_path"
)

validated_data_no_passed <- dplyr::tibble()
attr(validated_data_no_passed, "assertr_errors") <- list(
  list(message = "This is error",
       num.violations = 1,
       validation_id = 1))
attr(validated_data_no_passed, "assertr_warnings") <- list(
  list(
    message = "This is warning",
    num.violations = 1,
    validation_id = 2))
attr(validated_data_no_passed, "assertr_results") <- list(
  data.frame(
    title = "Error",
    result = FALSE,
    validation_id = 1,
    stringsAsFactors = FALSE),
  data.frame(
    title = "Warning",
    result = FALSE,
    validation_id = 2,
    stringsAsFactors = FALSE))

n_failed_no_passed <- n_warned_no_passed <- 1
n_passed_no_passed <- 0

validation_results_no_passed <- dplyr::tibble(
  title = c("Error", "Warning"),
  result = c("Failed", "Warning"),
  validation_id = as.numeric(1:2),
  object = rep("validated_data_no_passed", 2),
  file_path = rep("sample_file_path", 2),
  message = c("This is error", "This is warning"),
  num.violations = c(1, 1)
)
