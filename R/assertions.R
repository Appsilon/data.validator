#' Validate
#'
#' Prepared data for validation and generating report.
#'
#' @param data data.frame or tibble to test
#'
#' @param name name of validation object (will go to report)
#' @param description description of validation object (will go to report)
#'
#' @export
validate <- function(data, name, description = NULL) {
  if (missing(name)) {
    name <- deparse(substitute(data))
    if (name == ".") {
      name <- get_first_name(data)
    }
  }
  attr(data, "data-description") <- description
  attr(data, "data-name") <- name
  attr(data, "assertr_in_chain_success_fun_override") <- assertr::success_append
  attr(data, "assertr_in_chain_error_fun_override") <- assertr::error_append
  data
}

#' Match proper method depending on predicate type
#'
#' @param predicate Predicate or predicate generator function.
#' @param method optional list with fields direct and generator of assertions
get_assert_method <- function(predicate, method = list(direct = assertr::assert, generator = assertr::insist)) {
  predicate_output <- predicate(0:1)
  if (is.logical(predicate_output)) {
    return(method$direct)
  }
  if (is.function(predicate_output) && is.logical(predicate_output(0:1))) {
    return(method$generator)
  }
  stop("Predicate should be a function returning logical vector or function")
}

#' "If" Assertion
#'
#' Check if an assertion is met.
#'
#' @param data data.frame or tibble to test
#'
#' @param expr expression to test for, e.g. \code{within_n_sds(3)}
#' @param description character with description of assertion
#' @param obligatory flag setting assertion to mandatory
#' @param skip_chain_opts flag to skip chain options
#' @param success_fun function that is called after success
#' @param error_fun function that is called after error
#' @param defect_fun function that is called after defect
#'
#' @export
assert_if <- function(data, expr, description = NA, obligatory = FALSE, skip_chain_opts = FALSE,
                      success_fun = assertr::success_append,
                      error_fun = assertr::error_append, defect_fun = assertr::defect_append) {
  assertr::verify(data = data,
                  expr = !!rlang::enexpr(expr),
                  description = description,
                  skip_chain_opts = skip_chain_opts,
                  obligatory = obligatory,
                  success_fun = success_fun,
                  error_fun = error_fun,
                  defect_fun = defect_fun)
}

#' Assertion on columns
#'
#' @param data data.frame or tibble to test
#'
#' @param predicate  Predicate functions from asserts package, e.g. \code{in_set},
#' \code{within_bounds}, etc.
#' @param ... other arguments to predicate function
#' @param description character with description of assertion
#' @param obligatory flag setting assertion to mandatory
#' @param skip_chain_opts flag to skip chain options
#' @param success_fun function that is called after success
#' @param error_fun function that is called after error
#' @param defect_fun function that is called after defect
#'
#' @export
assert_cols <- function(data, predicate, ..., obligatory = FALSE, description = NA,
                        skip_chain_opts = FALSE, success_fun = assertr::success_append, error_fun = assertr::error_append,
                        defect_fun = assertr::defect_append) {

  assertr_function <- get_assert_method(predicate)

  assertr_function(
    data,
    predicate,
    ...,
    skip_chain_opts = skip_chain_opts,
    obligatory = obligatory,
    description = description,
    success_fun = success_fun,
    error_fun = error_fun,
    defect_fun = defect_fun
  )
}

#' Assertion on rows
#'
#' @param data data.frame or tibble to test
#'
#' @param row_reduction_fn function for row reduction
#' @param predicate  Predicate functions from asserts package, e.g. \code{in_set},
#' \code{within_bounds}, etc.
#' @param ... other arguments to predicate function
#' @param description character with description of assertion
#' @param predicate_generator predicate generator function.
#' @param obligatory flag setting assertion to mandatory
#' @param skip_chain_opts flag to skip chain options
#' @param success_fun function that is called after success
#' @param error_fun function that is called after error
#' @param defect_fun function that is called after defect
#' @export
assert_rows <- function(data, row_reduction_fn, predicate, ..., predicate_generator = NULL, obligatory = FALSE, description = NA,
                        skip_chain_opts = FALSE, success_fun = assertr::success_append, error_fun = assertr::error_append,
                        defect_fun = assertr::defect_append) {

  assertr_function <- get_assert_method(predicate, list(direct = assertr::assert_rows, generator = assertr::insist_rows))

  assertr_function(
      data,
      row_reduction_fn,
      predicate,
      ...,
      skip_chain_opts = skip_chain_opts,
      obligatory = obligatory,
      description = description,
      success_fun = success_fun,
      error_fun = error_fun,
      defect_fun = defect_fun
  )
}
