#' Default validator object
.validator <- create_validator()

#' Get default validator object
#' @export
getDefaultValidator <- function() {
  .validator
}

#' @export
validate <- function(data, name, description = NULL, validator = getDefaultValidator()) {
  # todo clear previous errors
  if (missing(name)) {
    name <- deparse(substitute(data))
    if (name == ".") {
      name <- get_first_name(data)
    }
  }
  attr(data, "data-description") <- description
  attr(data, "data-validator") <- validator
  attr(data, "assertr_in_chain_success_fun_override") <- success_append
  attr(data, "assertr_in_chain_error_fun_override") <- error_append
  attr(data, "data-name") <- name
  data
}

#' @export
assert_if <- function(data, expr, success_fun = success_append, error_fun = error_append,
                      skip_chain_opts = FALSE, obligatory = FALSE, defect_fun = defect_append,
                      description = NA) {
  assertr::verify(data = data, expr = !!rlang::enexpr(expr), success_fun = success_fun, error_fun = error_fun,
                  skip_chain_opts = skip_chain_opts, obligatory = obligatory, defect_fun = defect_fun,
                  description = description) %>%
    add_results(validator = attr(data, "data-validator"), attr(data, "data-name"))
}

#' @export
assert_cols <- function(data, predicate, ..., col_based = FALSE, success_fun = success_append,
                        error_fun = error_append, skip_chain_opts = FALSE, obligatory = FALSE,
                        defect_fun = defect_append, description = NA) {
  if (col_based) {
    data <- assertr::insist(
      data = data, predicate_generator = predicate, ...,
      success_fun = success_fun, error_fun = error_fun,
      skip_chain_opts = skip_chain_opts, obligatory = obligatory, defect_fun = defect_fun,
      description = description
    )
  } else {
    data <- assertr::assert(
      data = data, predicate = predicate, ...,
      success_fun = success_fun, error_fun = error_fun,
      skip_chain_opts = skip_chain_opts, obligatory = obligatory, defect_fun = defect_fun,
      description = description
    )
  }
  add_results(data, validator = attr(data, "data-validator"), attr(data, "data-name"))
}

#' @export
assert_rows <- function(data, row_reduction_fn, predicate, ..., col_based = FALSE, success_fun = success_append,
                        error_fun = error_append, skip_chain_opts = FALSE, obligatory = FALSE,
                        defect_fun = defect_append, description = NA) {
  if (col_based) {
    data <- assertr::insist_rows(
      data = data, row_reduction_fn = row_reduction_fn, predicate_generator = predicate, ...,
      success_fun = success_fun, error_fun = error_fun,
      skip_chain_opts = skip_chain_opts, obligatory = obligatory, defect_fun = defect_fun,
      description = description
    )
  } else {
    data <- assertr::assert_rows(
      data = data, row_reduction_fn = row_reduction_fn, predicate = predicate, ...,
      success_fun = success_fun, error_fun = error_fun,
      skip_chain_opts = skip_chain_opts, obligatory = obligatory, defect_fun = defect_fun,
      description = description
    )
  }
  add_results(data, validator = attr(data, "data-validator"), attr(data, "data-name"))
}
