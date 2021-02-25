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
#' @param methods List of two elements named 'direct' and 'generator' that will be returned depending on predicate type.

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

#' @export
assert_if <- function(data, expr, description = NA, obligatory = FALSE, skip_chain_opts = FALSE,
                      success_fun = assertr::success_append, error_fun = assertr::error_append, defect_fun = assertr::defect_append) {
  assertr::verify(data = data,
                  expr = !!rlang::enexpr(expr),
                  description = description,
                  skip_chain_opts = skip_chain_opts,
                  obligatory = obligatory,
                  success_fun = success_fun,
                  error_fun = error_fun,
                  defect_fun = defect_fun)
}

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
