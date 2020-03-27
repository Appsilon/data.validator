error_class <- "assertr_error"
success_class <- "assertr_success"

error_id <- "error"
success_id <- "success"
warning_id <- "warning"

get_assertion_type <- function(assertion) {
  assertr_types <- class(assertion)
  dplyr::case_when(
    success_class %in% assertr_types ~ success_id,
    isTRUE(attr(assertion, "warning")) ~ warning_id,
    error_class %in% assertr_types ~ error_id,
    TRUE ~ NA_character_
  )
}

parse_errors_to_df <- function(data) {
  if (is.null(attr(data, error_class))) {
    return(NULL)
  }
  attr(data, error_class) %>%
    purrr::map_df(
      ~ tibble::tibble(
        assertion.id = .$assertion.id,
        description = .$description,
        num.violations = .$num.violations,
        call = .$call,
        message = .$message,
        type = get_assertion_type(.),
        error_df = list(.$error_df)
      )
    ) %>%
    dplyr::group_by(assertion.id, description) %>%
    dplyr::mutate(type = type[1]) %>% # fixes simple assertr warning assignment
    dplyr::ungroup()
}

parse_successes_to_df <- function(data) {
  if (is.null(attr(data, success_class))) {
    return(NULL)
  }
  attr(data, success_class) %>%
    purrr::map_df(
      ~ tibble::tibble(
        assertion.id = generate_id(),
        description = .$description,
        num.violations = NA,
        call = .$call,
        message = .$message,
        type = get_assertion_type(.),
        error_df = list(NULL)
      )
    )
}

parse_results_to_df <- function(data) {
  dplyr::bind_rows(
    parse_errors_to_df(data),
    parse_successes_to_df(data)
  )
}

get_results_number <- function(results) {
  results %>%
    dplyr::select(assertion.id, type) %>%
    dplyr::distinct() %>%
    dplyr::pull(type) %>%
    table()
}
