Report <- R6::R6Class(
  classname = "Report",
  public = list(
    print = function(success = TRUE, warning = TRUE, error = TRUE) {
      types <- c(success_id, warning_id, error_id)[c(success, warning, error)]
      cat("Validation summary: \n")
      if (success) cat(" Number of successful validations: ", private$n_passed, "\n", sep = "")
      if (warning) cat(" Number of failed validations: ", private$n_failed, "\n", sep = "")
      if (error) cat(" Number of validations with warnings: ", private$n_warned, "\n", sep = "")
      if (nrow(private$validation_results) > 0) {
        cat("\n")
        cat("Advanced view: \n")
        print(private$validation_results %>%
                dplyr::filter(type %in% types) %>%
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
        dplyr::select(table_name, dplyr::everything())
      n_results <- get_results_number(results)
      private$n_failed <- sum(private$n_failed, n_results[error_id], na.rm = TRUE)
      private$n_warned <- sum(private$n_warned, n_results[warning_id], na.rm = TRUE)
      private$n_passed <- sum(private$n_passed, n_results[success_id], na.rm = TRUE)
      private$validation_results <- dplyr::bind_rows(private$validation_results, results)
      invisible(data)
    },
    get_validations = function(unnest = FALSE) {
      validation_results = private$validation_results
      if (unnest) {
        if (all(purrr::map_lgl(validation_results$error_df, is.null))) {
          validation_results$error_df <- NULL
          return(validation_results)
        }
        validation_results <- validation_results %>%
          tidyr::unnest(error_df, keep_empty = TRUE)
      }
      validation_results
    },
    generate_html_report = function(extra_params) {
      params_list <- modifyList(list(validation_results = private$validation_results), extra_params)
      do.call(private$report_constructor, params_list)
    },
    save_html_report = function(
      template = system.file("rmarkdown/templates/standard/skeleton/skeleton.Rmd", package = "data.validator"),
      output_file = "validation_report.html", output_dir = getwd(), report_ui_constructor = render_semantic_report_ui,
      ...) {

      private$report_constructor <- report_ui_constructor

      rmarkdown::render(
        input = template,
        output_format = "html_document",
        output_file = output_file,
        output_dir = output_dir,
        knit_root_dir = getwd(),
        params = list(
          generate_report_html = self$generate_html_report,
          extra_params = list(...)
        ),
        quiet = TRUE
      )
    },
    save_log = function(file_name = "validation_log.txt", success, warning, error) {
        sink(file_name)
        self$print(success, warning, error)
        sink()
    },
    save_results = function(file_name, method = write.csv, ...) {
      self$get_validations(unnest = TRUE) %>%
        write.csv(file = file_name)
    }
  ),
  private = list(
    n_failed = 0,
    n_passed = 0,
    n_warned = 0,
    validation_results = dplyr::tibble(),
    report_constructor = NULL
  )
)

#' Create new validator object
#'
#' @description  The object returns R6 class environment resposible for storing validation results.
#' @export
data_validation_report <- function() {
  Report$new()
}

#' Add validation results to the Report object
#'
#' @description This function adds results to validator object with aggregating summary of
#'   success, error and warning checks. Moreover it parses assertr results attributes and stores
#'   them inside usable table.
#'
#' @param data Data that was validated.
#' @param report Report object to store validation results.
#' @export
add_results <- function(data, report) {
  report$add_validations(data, name = attr(data, "data-name"))
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
#' @param report Report object that stores validation results. See \link{add_results}.
#' @param unnest If TRUE, error_df table is unnested. Results with remaining columns duplicated in table.
#' @export
get_results <- function(report, unnest = FALSE) {
  report$get_validations(unnest)
}

#' Saving results table to external file
#'
#' @param report Report object that stores validation results. See \link{get_results}.
#' @param file_name Name of the resulting file (including extension).
#' @param method Function that should be used to save results table (write.csv default).
#' @param ... Remaining parameters passed to \code{method}.
#' @export
save_results <- function(report, file_name = "results.csv", method = utils::write.csv, ...) {
  report$save_results(file_name, method, ...)
}

#' Saving results as a HTML report
#'
#' @param report Report object that stores validation results.
#' @param output_file Html file name to write report to.
#' @param output_dir Target report directory.
#' @param ui_constructor Function of \code{validation_results} and optional parameters that generates HTML
#'   code or HTML widget that should be used to generate report content. See \code{custom_report} example.
#' @param template Path to Rmd template in which ui_contructor is rendered. See \code{data.validator} rmarkdown
#'   template to see basic construction - the one is used as a default template.
#' @param ... Additional parameters passed to \code{ui_constructor}.
#' @export
save_report <- function(report, output_file = "validation_report.html", output_dir = getwd(), ui_constructor = render_semantic_report_ui,
                        template = system.file("rmarkdown/templates/standard/skeleton/skeleton.Rmd", package = "data.validator"), ...) {
  report$save_html_report(template, output_file, output_dir, ui_constructor, ...)
}

#' Save simple validation summary in text file
#'
#' @description Saves \code{print(validator)} output inside text file.
#' @param report Report object that stores validation results.
#' @param file_name Name of the resulting file (including extension).
#' @param success Should success results be presented?
#' @param warning Should warning results be presented?
#' @param error Should error results be presented?
#' @export
save_summary <- function(report, file_name = "validation_log.txt", success = TRUE, warning = TRUE, error = TRUE) {
  report$save_log(file_name, success, warning, error)
}
