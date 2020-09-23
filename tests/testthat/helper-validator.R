validated_data <- dplyr::tibble()
attr(validated_data, "assertr_errors") <- list(
  list(
    error_df = data.frame(
      verb = factor("assert"),
      redux_fn = NA,
      predicate = "validation_call()",
      column = factor("variable"),
      index = 1L,
      value = 0
    ),
    message = "column variable violates assertion",
    num.violations = 1L,
    call = "validation_call()",
    description = "variable correct",
    assertion.id = "a1"
    ),
  list(
    error_df = data.frame(
      verb = factor("assert"),
      redux_fn = NA,
      predicate = "validation_call()",
      column = factor("variable2"),
      index = 1L,
      value = 0
    ),
    message = "column variable2 violates assertion",
    num.violations = 1L,
    call = "validation_call()",
    description = "variable2 correct",
    assertion.id = "a1"
  )
)
attr(attr(validated_data, "assertr_errors")[[1]], "class") <- c("assertr_assert_error", "assertr_error", "error", "condition")
attr(attr(validated_data, "assertr_errors")[[2]], "class") <- c("assertr_assert_error", "assertr_error", "error", "condition")
attr(attr(validated_data, "assertr_errors")[[2]], "warning") <- TRUE
attr(validated_data, "assertr_success") <- list(
  list(
    verb = "assert",
    message = "column variable3 correct",
    call = "validation_call()",
    columns = "variable3",
    row_redux_call = NA,
    description = "variable3 correct"
  ),
  list(
    verb = "assert",
    message = "column variable4 correct",
    call = "validation_call()",
    columns = "variable4",
    row_redux_call = NA,
    description = "variable4 correct"
  )
)
attr(attr(validated_data, "assertr_success")[[1]], "class") <- c("assertr_success", "success", "condition")
attr(attr(validated_data, "assertr_success")[[2]], "class") <- c("assertr_success", "success", "condition")

validated_data_no_errors <- dplyr::tibble()
attr(validated_data_no_errors, "assertr_errors") <- list(
  list(
    error_df = data.frame(
      verb = factor("assert"),
      redux_fn = NA,
      predicate = "validation_call()",
      column = factor("variable"),
      index = 1L,
      value = 0
    ),
    message = "column variable violates assertion",
    num.violations = 1L,
    call = "validation_call()",
    description = "variable correct",
    assertion.id = "a1"
  )
)
attr(attr(validated_data_no_errors, "assertr_errors")[[1]], "class") <- c("assertr_assert_error", "assertr_error", "error", "condition")
attr(attr(validated_data_no_errors, "assertr_errors")[[1]], "warning") <- TRUE

attr(validated_data_no_errors, "assertr_success") <- list(
  list(
    verb = "assert",
    message = "column variable3 correct",
    call = "validation_call()",
    columns = "variable3",
    row_redux_call = NA,
    description = "variable3 correct"
  ),
  list(
    verb = "assert",
    message = "column variable4 correct",
    call = "validation_call()",
    columns = "variable4",
    row_redux_call = NA,
    description = "variable4 correct"
  )
)
attr(attr(validated_data_no_errors, "assertr_success")[[1]], "class") <- c("assertr_success", "success", "condition")
attr(attr(validated_data_no_errors, "assertr_success")[[2]], "class") <- c("assertr_success", "success", "condition")

validated_data_only_warnings <- dplyr::tibble()
attr(validated_data_only_warnings, "assertr_errors") <- list(
  list(
    error_df = data.frame(
      verb = factor("assert"),
      redux_fn = NA,
      predicate = "validation_call()",
      column = factor("variable"),
      index = 1L,
      value = 0
    ),
    message = "column variable violates assertion",
    num.violations = 1L,
    call = "validation_call()",
    description = "variable correct",
    assertion.id = "a1"
  )
)
attr(attr(validated_data_only_warnings, "assertr_errors")[[1]], "class") <- c("assertr_assert_error", "assertr_error", "error", "condition")
attr(attr(validated_data_only_warnings, "assertr_errors")[[1]], "warning") <- TRUE

validated_data_only_passed <- dplyr::tibble()
attr(validated_data_only_passed, "assertr_success") <- list(
  list(
    verb = "assert",
    message = "column variable3 correct",
    call = "validation_call()",
    columns = "variable3",
    row_redux_call = NA,
    description = "variable3 correct"
  ),
  list(
    verb = "assert",
    message = "column variable4 correct",
    call = "validation_call()",
    columns = "variable4",
    row_redux_call = NA,
    description = "variable4 correct"
  )
)
attr(attr(validated_data_only_passed, "assertr_success")[[1]], "class") <- c("assertr_success", "success", "condition")
attr(attr(validated_data_only_passed, "assertr_success")[[2]], "class") <- c("assertr_success", "success", "condition")

validated_data_no_passed <- dplyr::tibble()
attr(validated_data_no_passed, "assertr_errors") <- list(
  list(
    error_df = data.frame(
      verb = factor("assert"),
      redux_fn = NA,
      predicate = "validation_call()",
      column = factor("variable"),
      index = 1L,
      value = 0
    ),
    message = "column variable violates assertion",
    num.violations = 1L,
    call = "validation_call()",
    description = "variable correct",
    assertion.id = "a1"
  ),
  list(
    error_df = data.frame(
      verb = factor("assert"),
      redux_fn = NA,
      predicate = "validation_call()",
      column = factor("variable2"),
      index = 1L,
      value = 0
    ),
    message = "column variable2 violates assertion",
    num.violations = 1L,
    call = "validation_call()",
    description = "variable2 correct",
    assertion.id = "a1"
  )
)
attr(attr(validated_data_no_passed, "assertr_errors")[[1]], "class") <- c("assertr_assert_error", "assertr_error", "error", "condition")
attr(attr(validated_data_no_passed, "assertr_errors")[[2]], "class") <- c("assertr_assert_error", "assertr_error", "error", "condition")
attr(attr(validated_data_no_passed, "assertr_errors")[[2]], "warning") <- TRUE

private <- function(r6_object, el) {
  r6_object$.__enclos_env__$private[[el]]
}
