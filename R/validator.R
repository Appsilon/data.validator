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

#' @export
create_validator <- function() {
  Validator$new()
}

#' @export
add_results <- function(data, validator, name = NULL) {
  validator$add_validations(data, name)
}

#' @export
get_results <- function(validator, unnest = FALSE) {
  validator$get_validations(unnest)
}

#' @export
save_results <- function(validator, file_name = "results.csv", method = write.csv, ...) {
  validator$save_results(file_name, method, ...)
}

#' @export
save_report <- function(validator, output_file = "validation_report.html", output_dir = getwd(),
  summary = c("error", "warning", "success"), ui_constructor = render_semantic_report_ui,
  template = system.file("rmarkdown/templates/standard/skeleton/skeleton.Rmd", package = "datavalidator")) {

  validator$save_html_report(template, output_file, output_dir, summary, ui_constructor)
}

#' @export
save_summary <- function(validator, file_name = "validation_log.txt") {
  validator$save_log(file_name)
}
