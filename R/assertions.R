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

#' @export
assert_if <- function(data, expr, description = NA, obligatory = FALSE) {
  assertr::verify(data = data,
                  expr = !!rlang::enexpr(expr),
                  description = format_description(description),
                  skip_chain_opts = FALSE,
                  obligatory = obligatory,
                  success_fun = assertr::success_append,
                  error_fun = assertr::error_append,
                  defect_fun = assertr::defect_append)
}

#' @export
assert_cols <- function(data, cols, predicate = NULL, predicate_generator = NULL, obligatory = FALSE, description = NA) {
  assertr_function <- if (!is.null(predicate_generator)) assertr::insist else assertr::assert
  predicate_arg <- if (!is.null(predicate_generator)) predicate_generator else predicate

  assertr_function(
    data,
    predicate_arg,
    cols,
    skip_chain_opts = FALSE,
    obligatory = obligatory,
    description = description,
    success_fun = assertr::success_append,
    error_fun = assertr::error_append,
    defect_fun = assertr::defect_append
  )
}

#' @export
assert_rows <- function(data, cols, row_reduction_fn, predicate = NULL, predicate_generator = NULL, obligatory = FALSE, description = NA) {
  assertr_function <- if (!is.null(predicate_generator)) assertr::insist_rows else assertr::assert_rows
  predicate_arg <- if (!is.null(predicate_generator)) predicate_generator else predicate

  assertr_function(
      data,
      row_reduction_fn,
      predicate_arg,
      cols,
      skip_chain_opts = FALSE,
      obligatory = obligatory,
      description = description,
      success_fun = assertr::success_append,
      error_fun = assertr::error_append,
      defect_fun = assertr::defect_append
  )
}
