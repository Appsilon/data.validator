Validator <- R6::R6Class(
  classname = "Validator",
  public = list(
    print = function(...) {
      cat("\n")
      cat("Validation summary: \n")
      cat(" Number of passed validations: ", private$n_passed, "\n", sep = "")
      cat(" Number of failed validations: ", private$n_failed, "\n", sep = "")
      cat(" Number of validations with warnings: ", private$n_warned, "\n", sep = "")
      cat("\n")
      if (nrow(private$validation_results) > 0) {
        cat("Advanced view: \n")
        print(private$validation_results %>%
                dplyr::select(object, title, result, validation_id) %>%
                knitr::kable())
      }
      invisible(self)
    },
    add_validations = function(data, name = NULL) {
      object_name <- ifelse(!is.null(name), name, get_first_name(data))
      results <- parse_results_to_df(data) %>%
        mutate(table_name = object_name) %>%
        select(table_name, everything())
      n_results <- get_results_number(results)
      private$n_failed <- sum(private$n_failed, n_results[error_id], na.rm = TRUE)
      private$n_warned <- sum(private$n_warned, n_results[warning_id], na.rm = TRUE)
      private$n_passed <- sum(private$n_passed, n_results[success_id], na.rm = TRUE)
      private$validation_results <- dplyr::bind_rows(private$validation_results, results)
      invisible(self)
    },
    get_validations = function() {
      list(n_failed = private$n_failed, n_warned = private$n_warned,
           n_passed = private$n_passed, validation_results = private$validation_results)
    },
    generate_html_report = function(summary, render_report_ui) {
      n_passed <- NULL
      n_warned <- NULL
      n_failed <- NULL
      if (success_id %in% summary) n_passed <- private$n_passed
      if (warning_id %in% summary) n_warned <- private$n_warned
      if (error_id %in% summary) n_failed <- private$n_failed
      validation_results <- private$validation_results %>%
        filter(type %in% summary)
      render_report_ui(n_passed, n_failed, n_warned, validation_results)
    },
    save_html_report = function(
      template = system.file("rmarkdown/templates/semantic/skeleton/skeleton.Rmd", package = "datavalidator"),
      output_file = "validation_report.html", output_dir = getwd(), summary = c("error", "warning", "success"),
      render_report_ui = render_semantic_report_ui) {

      rmarkdown::render(
        input = template,
        output_format = "html_document", output_file = output_file,
        output_dir = output_dir,
        params = list(
          generate_report_html = self$generate_html_report,
          summary = summary,
          render_report_ui = render_report_ui
        )
      )
    },
    save_log = function(file_name = "validation_log", type = "txt") {
      full_path <- paste0(file_name, ".", type)
      if (type == "txt") {
        sink(full_path)
        self$print()
        sink()
      }
      if (type == "csv") {
        private$validation_results %>%
          tidyr::unnest(error_df, keep_empty = TRUE) %>%
          write.csv(file = full_path)
      }
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
get_results <- function(validator) {
  validator$get_validations()
}

#' @export
save_report <- function(validator, output_file = "validation_report.html", output_dir = getwd(),
  summary = c("error", "warning", "success"), render_report_ui = render_semantic_report_ui,
  template = system.file("rmarkdown/templates/semantic/skeleton/skeleton.Rmd", package = "datavalidator")) {

  validator$save_html_report(template, output_file, output_dir, summary, render_report_ui)
}

#' @export
save_summary <- function(validator, file_name = "validation_log", type = "txt") {
  validator$save_log(file_name, type)
}
