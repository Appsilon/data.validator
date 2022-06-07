#' Prepare data for validation chain
#'
#' Prepare data for validation and generating report.
#' The function prepares data for chain validation and ensures all the validation results are gathered correctly.
#' The function also attaches additional information to the data (name and description)
#' that is then displayed in validation report.
#'
#' @param data data.frame or tibble to test
#'
#' @param name name of validation object (will be displayed in the report)
#' @param description description of validation object (will be displayed in the report)
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

#' Defensive wrapper to add evaluation error to regular validation errors.
#'
#' @param this_call assertion command that is checked for valuation errors.
#' @param data A data.frame or tibble to test.
#' @param description A character string with description of assertion.
#' @param error_fun Function that is called when the validation fails
#'
#' @return
#'
#' @examples
#' \dontrun{
#' library(fixtuRes)
#' library(magrittr)
#' library(assertr)
#' library(data.validator)
#'
#' my_mock_generator <- fixtuRes::MockDataGenerator$new("fixtures_config.yml")
#' my_data_frame <- my_mock_generator$get_data("my_data_frame", 10)
#'
#' report <- data.validator::data_validation_report()
#'
#' validate(my_data_frame, name = "Verifying data uniqueness") %>%
#'   validate_if(has_all_names("id", "code", "test"), description = "All columns are there") %>%
#'   validate_if(is.character(test), description = "TEST column is string") %>%
#'   validate_if(is_uniq(id), description = "ID column is unique") %>%
#'   validate_if(!is.na(id) & id != "", description = "ID column is not empty") %>%
#'   validate_if(is.character(code), description = "CODE column is string") %>%
#'   validate_rows(col_concat, is_uniq, code, type, description = "CODE and TYPE combination is unique") %>%
#'   add_results(report)
#'
#' print(report)
#' }
#' @seealso validate_if
check_assertr_expression <- function(this_call, data, description, error_fun) {
  tryCatch(
    this_call,
    error = function(e) {
      current_error <- assertr:::make.assertr.verify.error(
        "verify", num.violations = 1, the_call = as.character(e),
        logical.results = 1, description, assertr:::generate_id()
      )
      data <- error_fun(list(current_error), data)
      return(data)
    }
  )
}

#' Verify if expression regarding data is TRUE
#'
#' The function checks whether all the logical values returned by the expression are TRUE.
#' The function is meant for handling all the cases that cannot be reached by using \link{validate_cols}
#' and \link{validate_rows} functions.
#'
#' @param data A data.frame or tibble to test
#' @param expr A Logical expression to test for, e.g. \code{var_name > 0}
#' @param description A character string with description of assertion.
#'   The description is then displayed in the validation report
#' @param obligatory If TRUE and assertion failed the data is marked as defective.
#'   For defective data, all the following rules are handled by defect_fun function
#' @param skip_chain_opts While wrapping data with \link{validate} function, \code{success_fun} and
#'   \code{error_fun} parameters are rewritten with \code{success_append} and \code{error_append} respectively.
#'   In order to use parameters assigned to the function directly set skip_chain_opts to TRUE
#' @param success_fun Function that is called when the validation pass
#' @param error_fun Function that is called when the validation fails
#' @param defect_fun Function that is called when the data is marked as defective
#'
#' @export
#' @seealso validate_cols validate_rows
validate_if <- function(data, expr, description = NA, obligatory = FALSE, skip_chain_opts = FALSE,
                        success_fun = assertr::success_append,
                        error_fun = assertr::error_append, defect_fun = assertr::defect_append) {

  assertr::verify(data = data, expr = !!rlang::enexpr(expr),
                  description = description, skip_chain_opts = skip_chain_opts,
                  obligatory = obligatory, success_fun = success_fun,
                  error_fun = error_fun, defect_fun = defect_fun) %>%
    check_assertr_expression(data, description, error_fun)
}

#' Validation on columns
#'
#' @param data A data.frame or tibble to test
#' @param predicate  Predicate function or predicate generator such as \code{\link[assertr]{in_set}} or
#'   \code{\link[assertr]{within_n_sds}}
#' @param ... Columns selection that \code{predicate} should be called on.
#'   All tidyselect \code{\link[tidyselect]{language}} methods are supported
#' @param description A character string with description of assertion.
#'   The description is then displayed in the validation report
#' @param obligatory If TRUE and assertion failed the data is marked as defective.
#'   For defective data, all the following rules are handled by defect_fun function
#' @param skip_chain_opts While wrapping data with \link{validate} function, \code{success_fun} and
#'   \code{error_fun} parameters are rewritten with \code{success_append} and \code{error_append} respectively.
#'   In order to use parameters assigned to the function directly set skip_chain_opts to TRUE
#' @param success_fun Function that is called when the validation pass
#' @param error_fun Function that is called when the validation fails
#' @param defect_fun Function that is called when the data is marked as defective
#'
#' @export
#' @seealso validate_if validate_rows
validate_cols <- function(data, predicate, ..., obligatory = FALSE, description = NA,
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
  ) %>%
    check_assertr_expression(data, description, error_fun)
}

#' Validation on rows
#'
#' @param data A data.frame or tibble to test
#' @param row_reduction_fn Function that should reduce rows into a single column that is passed to validation
#'   e.g. \code{\link[assertr]{num_row_NAs}}
#' @param predicate  Predicate function or predicate generator such as \code{\link[assertr]{in_set}} or
#'   \code{\link[assertr]{within_n_sds}}
#' @param ... Columns selection that \code{row_reduction_fn} should be called on.
#'   All tidyselect \code{\link[tidyselect]{language}} methods are supported
#' @param description A character string with description of assertion.
#'   The description is then displayed in the validation report
#' @param obligatory If TRUE and assertion failed the data is marked as defective.
#'   For defective data, all the following rules are handled by defect_fun function
#' @param skip_chain_opts While wrapping data with \link{validate} function, \code{success_fun} and
#'   \code{error_fun} parameters are rewritten with \code{success_append} and \code{error_append} respectively.
#'   In order to use parameters assigned to the function directly set skip_chain_opts to TRUE.
#' @param success_fun Function that is called when the validation pass
#' @param error_fun Function that is called when the validation fails
#' @param defect_fun Function that is called when the data is marked as defective
#' @export
#' @seealso validate_cols validate_if
validate_rows <- function(data, row_reduction_fn, predicate, ..., obligatory = FALSE, description = NA,
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
  ) %>%
    check_assertr_expression(data, description, error_fun)
}
