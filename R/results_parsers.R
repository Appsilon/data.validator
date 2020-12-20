#' Constants
error_class <- "assertr_error"
success_class <- "assertr_success"

error_id <- "error"
success_id <- "success"
warning_id <- "warning"

#' get assertion type
#'
#' @param assertion assertion object (check \code{assertr} package for details)
#'
#' @return character with id of assertion: "error", "success", "warning"
get_assertion_type <- function(assertion) {
  assertr_types <- class(assertion)
  dplyr::case_when(
    success_class %in% assertr_types ~ success_id,
    isTRUE(attr(assertion, "warning")) ~ warning_id,
    error_class %in% assertr_types ~ error_id,
    TRUE ~ NA_character_
  )
}

#' Convert error table column types
#'
#' @param error_df Table consisting assertr error details
convert_error_df <- function(error_df) {
  dplyr::mutate_at(error_df, dplyr::vars(c("verb", "redux_fn", "predicate", "column", "value")), as.character)
}

#' Parse errors to data.frame
#'
#' @param data object of assertr error class  (check \code{assertr} package
#'  for details)
#'
#' @return data.frame with errors
parse_errors_to_df <- function(data) {
  if (is.null(attr(data, error_class))) {
    return(NULL)
  }
  attr(data, error_class) %>%
    purrr::map_df(
      ~ tibble::tibble(
        assertion.id = generate_id(), # TODO(pawel): .$assertion.id,  Error: Column `assertion.id` not found in `.data`
        description = .$description,
        num.violations = .$num.violations,
        call = .$call,
        message = .$message,
        type = get_assertion_type(.),
        error_df = list(convert_error_df(.$error_df))
      )
    ) %>%
    dplyr::group_by(.data$assertion.id, .data$description) %>%
    dplyr::mutate(type = .data$type[1]) %>% # fixes simple assertr warning assignment
    dplyr::ungroup()
}

#' Parse successes to data.frame
#'
#' @param data object of assertr success class  (check \code{assertr} package
#'  for details)
#'
#' @return data.frame with successes
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

#' Parse results to data.frame
#'
#' @param data assertr object  (check \code{assertr} package for details)
#'
#' @return data.frame with successes and errors
parse_results_to_df <- function(data) {
  dplyr::bind_rows(
    parse_errors_to_df(data),
    parse_successes_to_df(data)
  )
}

#' Get results number
#'
#' @param results assertion results
#'
#' @return table with results number
get_results_number <- function(results) {
  results %>%
    dplyr::select(.data$assertion.id, .data$type) %>%
    dplyr::distinct() %>%
    dplyr::pull(.data$type) %>%
    table()
}
