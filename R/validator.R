error_class <- "assertr_error"
success_class <- "assertr_success"

error_id <- "error"
success_id <- "success"
warning_id <- "warning"

get_assertion_type <- function(assertion) {
  assertr_types <- class(assertion)
  case_when(
    success_class %in% assertr_types ~ success_id,
    isTRUE(attr(assertion, "warning")) ~ warning_id,
    error_class %in% assertr_types ~ error_id,
    TRUE ~ NA_character_
  )
}

parse_errors_to_df <- function(data) {
  attr(data, error_class) %>%
    purrr::map_df(
      ~ tibble(
        assertion.id = .$assertion.id,
        description = .$description,
        num.violations = .$num.violations,
        call = .$call,
        message = .$message,
        type = get_assertion_type(.),
        error_df = list(.$error_df)
      )
    ) %>%
    group_by(assertion.id, description) %>%
    mutate(type = type[1]) %>% # fixes simple assertr warning assignment
    ungroup()
}

parse_successes_to_df <- function(data) {
  attr(data, success_class) %>%
    purrr::map_df(
      ~ tibble(
        assertion.id = assertr:::generate_id(),
        description = .$description,
        num.violations = NA,
        call = .$call,
        message = .$message,
        type = get_assertion_type(.),
        error_df = list(NULL)
      )
    )
}

parse_results_to_df <- function(data) {
  bind_rows(
    parse_errors_to_df(data),
    parse_successes_to_df(data)
  )
}

get_results_number <- function(results) {
  results %>%
    select(assertion.id, type) %>%
    distinct() %>%
    pull(type) %>%
    table()
}

find_chain_parts <- function() {
  i <- 1
  while(!("chain_parts" %in% ls(envir=parent.frame(i))) && i < sys.nframe()) {
    i <- i+1
  }
  parent.frame(i)
}

get_first_name <- function(df){
  ee <- find_chain_parts()
  deparse(ee$lhs)
}


#' Creates data frame from validation attribute.
#' @description Creates data frame from validation attribute.
#' @param data Validated data frame.
#' @param attribute Attribute name.
#' @return Data frame with validation results.
get_validations_attribute <- function(data, attribute) {
  attr(data, attribute) %>% purrr::map_df(~ dplyr::tibble(
    assertion.id = .$assertion.id,
    message = .$message,
    num.violations = .$num.violations,
    type = get_assertion_type(.)
    )
  )
}

#' Creates data frame from validation results.
#' @description Creates data frame from validation results.
#' @param data Validated data frame.
#' @param errors Data frame with validation errors. See \code{get_validations_attribute} function.
#' @param warnings Data frame with validation warnings. See \code{get_validations_attribute} function.
#' @param object_name Title to display in the report.
#' @return Data frame with validation results.
create_validation_results <- function(data, errors, warnings, object_name) {
  results <- attr(data, "assertr_results") %>% dplyr::bind_rows() %>%
    dplyr::mutate(object = object_name,
                  result = dplyr::case_when(
                    result == TRUE ~ "Passed",
                    TRUE ~ as.character(result)))
  if (nrow(errors) > 0) {
    results <- results %>% dplyr::mutate(result = dplyr::case_when(
      validation_id %in% errors$validation_id ~ "Failed",
      TRUE ~ as.character(result)))
  }
  if (!is.null(warnings) && nrow(warnings) > 0) {
    results <- results %>% dplyr::mutate(result = dplyr::case_when(
      validation_id %in% warnings$validation_id ~ "Warning",
      TRUE ~ as.character(result)))
  }
  errors_and_warnings <- dplyr::bind_rows(errors, warnings)
  if (nrow(errors_and_warnings) > 0) {
    dplyr::left_join(results, errors_and_warnings, by = "assertion.id")
  } else {
    results
  }
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
#' \item{\code{add_validations(data)}}{This method adds \code{assertr} validation results to the report.}
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
    generate_html_report = function(sum_up, render_report_ui) {
      n_passed <- ifelse(success_id %in% sum_up, private$n_passed, NULL)
      n_warned <- ifelse(warning_id %in% sum_up, private$n_warned, NULL)
      n_failed <- ifelse(error_id %in% sum_up, private$n_failed, NULL)
      validation_results <- private$validation_results %>%
        filter(type %in% sum_up)
      render_report_ui(n_passed, n_failed, n_warned, validation_results)
    },
    save_html_report = function(
      template = system.file("rmarkdown/templates/semantic/skeleton/skeleton.Rmd", package = "datavalidator"),
      output_file = "validation_report.html", output_dir = getwd(), sum_up = c("error", "warning", "success"),
      render_report_ui = render_semantic_report_ui) {

      rmarkdown::render(
        input = template,
        output_format = "html_document", output_file = output_file,
        output_dir = output_dir,
        params = list(
          generate_report_html = self$generate_html_report,
          sum_up = sum_up,
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
        self$validation_results %>%
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
  ))

#' Renders validation report.
#' @description Renders validation report.
#' @param template Report Rmd template.
#' @param output_dir Output directory for HTML report.
#' @param output_file Output name for HTML report.
#' @param scripts Vector containing paths for the scripts to run inside the template (e.g. data reading, validations).
#'   Note that the paths should be relative to the location of the template.
#' @param repo_path Github repo path.
#' @return Validation report.
#' @export
render_validation_report <- function(template, output_dir, output_file = "validation_report.html", scripts) {
  params = list(scripts = scripts)
  rmarkdown::render(
    input = template, output_format = "html_document", output_file = output_file,
    output_dir = output_dir, params = params
  )
}
