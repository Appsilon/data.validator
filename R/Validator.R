#' Creates data frame from validation attribute.
#' @description Creates data frame from validation attribute.
#' @param data Validated data frame.
#' @param attribute Attribute name.
#' @return Data frame with validation results.
get_validations_attribute <- function(data, attribute) {
  validations_attribute <- attr(data, attribute) %>% purrr::map_df(~ dplyr::tibble(
    validation_id = .$validation_id,
    message = .$message,
    num.violations = .$num.violations)
  )
  if (rlang::is_empty(validations_attribute) | is.null(validations_attribute)) {
    dplyr::tibble(validation_id = "no_cases_found")
  } else {
    validations_attribute
  }
}

#' Creates data frame from validation results.
#' @description Creates data frame from validation results.
#' @param data Validated data frame.
#' @param errors Data frame with validation errors. See \code{get_validations_attribute} function.
#' @param warnings Data frame with validation warnings. See \code{get_validations_attribute} function.
#' @param file_path Github repo file path to redirect.
#' @param object_name Title to display in the report.
#' @return Data frame with validation results.
create_validation_results <- function(data, errors, warnings, file_path, object_name) {
  attr(data, "assertr_results") %>% dplyr::bind_rows() %>%
    dplyr::mutate(object = object_name,
           file_path = ifelse(is.null(file_path), NA, file_path),
           result = dplyr::case_when(
             validation_id %in% errors$validation_id ~ "Failed",
             validation_id %in% warnings$validation_id ~ "Warning",
             TRUE ~ "Passed"
           )) %>%
    dplyr::left_join(dplyr::bind_rows(errors, warnings), by = "validation_id")
}

#' Class providing object with methods for simple data validation reports.
#' @docType class
#' @return Object of \code{\link{R6Class}} with methods for simple data validation reports.
#' @export
#' @keywords data
#' @format \code{\link{R6Class}} object.
#' @examples
#' validator <- Validator$new()
#' @field repo_path Github repository url of a project.
#' @section Methods:
#' \describe{
#' \item{\code{add_validations(data, file_path = NULL)}}{This method adds \code{assertr} validation results to the report.}
#' \item{\code{get_validations()}}{This method returns list of current validations.}
#' \item{\code{generate_html_report()}}{This method generates \code{HTML} validation report.}
#' \item{\code{save_log(output_path = "validation_log")}}{This method saves report log into \code{output_path}}}
Validator <- R6::R6Class(
  classname = "Validator",
  public = list(
    repo_path = NULL,
    print = function(...) {
      cat("Github repo path: ", ifelse(is.null(self$repo_path), "NOT DEFINED", self$repo_path), "\n")
      cat("\n")
      cat("Validation summary: \n")
      cat(" Number of passed validations: ", private$n_passed, "\n", sep = "")
      cat(" Number of failed validations: ", private$n_failed, "\n", sep = "")
      cat(" Number of validations with warnings: ", private$n_warned, "\n", sep = "")
      cat("\n")
      if (nrow(private$validation_results) > 0) {
        cat("Advanced view: \n")
        print(private$validation_results %>%
              select(object, title, result, validation_id) %>%
              knitr::kable())
      }
      invisible(self)
    },
    add_validations = function(data, file_path = NULL) {
      errors <- get_validations_attribute(data, "assertr_errors")
      warnings <- get_validations_attribute(data, "assertr_warnings")
      object_name <- ifelse(is.null(attr(data, "ribbon-title")), deparse(substitute(data)), attr(data, "ribbon-title"))
      results <- create_validation_results(data, errors, warnings, file_path, object_name)
      private$n_failed <- private$n_failed + nrow(errors)
      private$n_warned <- private$n_warned + nrow(warnings)
      private$n_passed <- private$n_passed + nrow(results) - nrow(errors) - nrow(warnings)
      private$validation_results <- bind_rows(private$validation_results, results)
      invisible(self)
    },
    get_validations = function() {
      list(n_failed = private$n_failed, n_warned = private$n_warned,
           n_passed = private$n_passed, validation_results = private$validation_results)
    },
    generate_html_report = function() {
      generate_html_report(private$n_passed, private$n_failed, private$n_warned,
                           private$validation_results, self$repo_path) %>%
        shiny.semantic::uirender(., width = "100%", height = "100%")
    },
    save_log = function(output_path = "validation_log") {
      sink(output_path)
      self$print()
      sink()
    }
  ),
  private = list(
    n_failed = 0,
    n_passed = 0,
    n_warned = 0,
    validation_results = dplyr::tibble()
  ))

#' Renders validation report.
#' @description Renders validation report.
#' @param template Report Rmd template.
#' @param output_dir Output directory for HTML report.
#' @param output_file Output name for HTML report.
#' @param scripts Vector containing paths for the scripts to run inside the template (e.g. data reading, validations). Note that the paths should be relative to the location of the template.
#' @param repo_path Github repo path.
#' @return Validation report.
#' @export
render_validation_report <- function(template, output_dir, output_file = "validation_report.html", scripts, repo_path = NULL) {
  params = list(repo_path = repo_path, scripts = scripts)
  rmarkdown::render(input = template, output_format = "html_document", output_file = output_file,
                    output_dir = output_dir, params = params)
  system(paste0('xdg-open "', output_dir, '/', output_file, '" > /dev/null'))
}
