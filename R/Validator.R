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
#' \item{\code{generate_html_report()}}{This method generates \code{HTML} validation report.}}
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
        print(private$validation_results %>% select(object, title, result, validation_id))
      }
      invisible(self)
    },
    add_validations = function(data, file_path = NULL) {
      errors <- attr(data, "assertr_errors") %>% purrr::map_df(~ tibble(
        validation_id = .$validation_id,
        message = .$message,
        num.violations = .$num.violations)
      )
      warnings <- attr(data, "assertr_warnings") %>% purrr::map_df(~ tibble(
        validation_id = .$validation_id,
        message = .$message,
        num.violations = .$num.violations)
      )
      object_name <- ifelse(is.null(attr(data, "ribbon-title")), deparse(substitute(data)), attr(data, "ribbon-title"))
      results <- attr(data, "assertr_results") %>% bind_rows() %>%
        mutate(object = object_name,
               file_path = ifelse(is.null(file_path), NA, file_path),
               result = case_when(
                 validation_id %in% errors$validation_id ~ "Failed",
                 validation_id %in% warnings$validation_id ~ "Warning",
                 TRUE ~ "Passed"
               )) %>%
        left_join(bind_rows(errors, warnings), by = "validation_id")
      private$n_failed <- private$n_failed + nrow(errors)
      private$n_warned <- private$n_warned + nrow(warnings)
      private$n_passed <- private$n_passed + nrow(results %>% filter(result == "Passed"))
      private$validation_results <- bind_rows(private$validation_results, results)
      invisible(self)
    },
    generate_html_report = function(...) {
      generate_html_report(private$n_passed, private$n_failed, private$n_warned,
                           private$validation_results, self$repo_path) %>%
        shiny.semantic::uirender(., width = "100%", height = "100%")
    }
  ),
  private = list(
    n_failed = 0,
    n_passed = 0,
    n_warned = 0,
    validation_results = dplyr::tibble()
  ))

#' Generate validation report.
#' @description Generate validation report.
#' @param template Report Rmd template.
#' @param output_dir Output directory for HTML report.
#' @param output_file Output name for HTML report.
#' @param scripts Vector containing paths for the scripts to run inside the template (e.g. data reading, validations). Note that the paths should be relative to the location of the template.
#' @param repo_path Github repo path.
#' @return Validation report.
#' @export
generate_report <- function(template, output_dir, output_file = "validation_report.html", scripts, repo_path = NULL) {
  params = list(repo_path = repo_path, scripts = scripts)
  rmarkdown::render(input = template, output_format = "html_document", output_file = output_file,
                    output_dir = output_dir, params = params)
  system(paste0('xdg-open "', output_dir, '/', output_file, '" > /dev/null'))
}
