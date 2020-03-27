Validator <- R6::R6Class(
  classname = "Validator",
  public = list(
    print = function(summary = c("error", "warning", "success")) {
      cat("\n")
      cat("Validation summary: \n")
      if (success_id %in% summary) cat(" Number of successful validations: ", private$n_passed, "\n", sep = "")
      if (error_id %in% summary) cat(" Number of failed validations: ", private$n_failed, "\n", sep = "")
      if (warning_id %in% summary) cat(" Number of validations with warnings: ", private$n_warned, "\n", sep = "")
      if (nrow(private$validation_results) > 0) {
        cat("Advanced view: \n")
        print(private$validation_results %>%
                dplyr::filter(type %in% summary) %>%
                dplyr::select(table_name, description, type, num.violations) %>%
                dplyr::group_by(table_name, description, type) %>%
                dplyr::summarise(total_violations = sum(num.violations)) %>%
                knitr::kable())
      }
      invisible(self)
    },
    add_validations = function(data, name = NULL) {
      object_name <- ifelse(!is.null(name), name, get_first_name(data))
      results <- parse_results_to_df(data) %>%
        dplyr::mutate(table_name = object_name) %>%
        dplyr::select(table_name, everything())
      n_results <- get_results_number(results)
      private$n_failed <- sum(private$n_failed, n_results[error_id], na.rm = TRUE)
      private$n_warned <- sum(private$n_warned, n_results[warning_id], na.rm = TRUE)
      private$n_passed <- sum(private$n_passed, n_results[success_id], na.rm = TRUE)
      private$validation_results <- dplyr::bind_rows(private$validation_results, results)
      invisible(self)
    },
    get_validations = function(unnest = FALSE) {
      results <- list(
        n_failed = private$n_failed,
        n_warned = private$n_warned,
        n_passed = private$n_passed,
        validation_results = private$validation_results
      )
      if (unnest) {
        results$validation_results <- results$validation_results %>%
          tidyr::unnest(error_df, keep_empty = TRUE)
      }
      results
    },
    generate_html_report = function(summary, render_report_ui) {
      n_passed <- NULL
      n_warned <- NULL
      n_failed <- NULL
      if (success_id %in% summary) n_passed <- private$n_passed
      if (warning_id %in% summary) n_warned <- private$n_warned
      if (error_id %in% summary) n_failed <- private$n_failed
      validation_results <- private$validation_results %>%
        dplyr::filter(type %in% summary)
      render_report_ui(n_passed, n_failed, n_warned, validation_results)
    },
    save_html_report = function(
      template = system.file("rmarkdown/templates/standard/skeleton/skeleton.Rmd", package = "datavalidator"),
      output_file = "validation_report.html", output_dir = getwd(), summary = c("error", "warning", "success"),
      report_ui_constructor = render_semantic_report_ui, ...) {

      rmarkdown::render(
        input = template,
        output_format = "html_document", output_file = output_file,
        output_dir = output_dir,
        knit_root_dir = getwd(),
        output_option = list(message = FALSE, warning = FALSE),
        params = list(
          generate_report_html = self$generate_html_report,
          summary = summary,
          report_ui_constructor = report_ui_constructor,
          ...
        )
      )
    },
    save_log = function(file_name = "validation_log.txt") {
        sink(file_name)
        self$print()
        sink()
    },
    save_results = function(file_name, method = write.csv, ...) {
      private$validation_results %>%
        tidyr::unnest(error_df, keep_empty = TRUE) %>%
        write.csv(file = file_name)
    }
  ),
  private = list(
    n_failed = 0,
    n_passed = 0,
    n_warned = 0,
    validation_results = dplyr::tibble()
  )
)

#' Create new validator object
#'
#' @description  The object returns R6 class environment resposible for storing validation results.
#' @export
create_validator <- function() {
  Validator$new()
}

#' Add validation results to validator object
#'
#' @description This function adds results to validator object with aggregating summary of
#'   success, error and warning checks. Moreover it parses assertr results attributes and stores
#'   them inside usable table.
#'
#' @param data Data that was validated.
#' @param validator Validator object to store results to created with \link{create_results}.
#' @param name Optional name for data that was validated.
#'   By default the name of first object used in validation chain.
#' @export
add_results <- function(data, validator, name = NULL) {
  validator$add_validations(data, name)
}

#' Get validation results
#'
#' @description The response is a list containing information about successful, failed, warning assertions and
#'   the table stores important information about validation results. Those are:
#'   \itemize{
#'     \item table_name - name of validated table
#'     \item assertion.id - id used for each assertion
#'     \item description - assertion description
#'     \item num.violations - number of violations (assertion and column specific)
#'     \item call - assertion call
#'     \item message - assertion result message for specific column
#'     \item type - error, warning or success
#'     \item error_df - nested table storing details about error or warning result (like vilated indexes and valies)
#'   }
#' @param validator Validator object that stores validation results. See \link{add_results}.
#' @param unnest If TRUE, error_df table is unnested. Results with remaining columns duplicated in table.
#' @export
get_results <- function(validator, unnest = FALSE) {
  validator$get_validations(unnest)
}

#' Saving results table to external file
#'
#' @param validator Validator object that stores validation results. See \link{get_results}.
#' @param file_name Name of the resulting file (including extension).
#' @param method Function that should be used to save results table (write.csv default).
#' @param ... Remaining parameters passed to \code{method}.
#' @export
save_results <- function(validator, file_name = "results.csv", method = write.csv, ...) {
  validator$save_results(file_name, method, ...)
}

#' Saving results as a HTML report
#'
#' @param validator Validator object that stores validation results.
#' @param output_file Html file name to write report to.
#' @param output_dir Report directory.
#' @param summary Vector storing at least one of \code{"error"}, \code{"warning"}, \code{"success"}.
#'   Results with using validation filtered out results table in report.
#'   Moreover, skipping one of the options sets up \code{n_failed}, \code{n_warned} or \code{n_passed}
#'   that is passed to \code{ui_contructor} function as NULL - helpful if you want to distinguish whether
#'   to include related information in report or not (comparing to value 0 that means there were no results
#'   of related type).
#' @param ui_contructor Function of four arguments \code{n_failed}, \code{n_warned} or \code{n_passed} and
#'   \code{validation_reults} that generates HTML code or HTML widget that should be used to generate report
#'   content. See \code{custom_report} example.
#' @param template Path to Rmd template in which ui_contructor is rendered. See \code{data.validator} rmarkdown
#'   template to see basic construction - the one is used as a default template.
#' @export
save_report <- function(validator, output_file = "validation_report.html", output_dir = getwd(),
  summary = c("error", "warning", "success"), ui_constructor = render_semantic_report_ui,
  template = system.file("rmarkdown/templates/standard/skeleton/skeleton.Rmd", package = "datavalidator")) {

  validator$save_html_report(template, output_file, output_dir, summary, ui_constructor)
}

#' Save simple validation summary in text file
#'
#' @description Saves \code{print(validator)} output inside text file.
#' @param validator Validator object that stores validation results.
#' @param file_name Name of the resulting file (including extension).
#' @export
save_summary <- function(validator, file_name = "validation_log.txt") {
  validator$save_log(file_name)
}
