validated_data <- dplyr::tibble()
attr(validated_data, "assertr_errors") <- list(list(message = "This is error",
                                                    num.violations = 1,
                                                    validation_id = 1))
attr(validated_data, "assertr_warnings") <- list(list(message = "This is warning",
                                                      num.violations = 1,
                                                      validation_id = 2))
attr(validated_data, "assertr_results") <- list(data.frame(title = "Error",
                                                           result = FALSE,
                                                           validation_id = 1,
                                                           stringsAsFactors = FALSE),
                                                data.frame(title = "Warning",
                                                           result = FALSE,
                                                           validation_id = 2,
                                                           stringsAsFactors = FALSE),
                                                data.frame(title = "Ok",
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
