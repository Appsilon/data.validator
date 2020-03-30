#' Create a UI segment element.
#' @description Create a UI segment element.
#' @param title Title of the segment.
#' @param ... Additional agruments inside segment.
#' @return Segment.
segment <- function(title, ...) {
  shiny::div(class = "ui raised segment", style = "margin-bottom: 0.5em",
             shiny::div(class = "ui demo ribbon label blue", title),
             ...
  )
}

#' Prepare modal content.
#' @description Prepare modal content.
#' @param error Assertr error.
#' @return Modal content.
prepare_modal_content <- function(error) {
  data_part <- NULL
  errors_number <- seq_along(error$error_df[[1]])
  purrr::map(errors_number, ~ {
    data_part <-
      shiny::div(style = "padding-left: 1em;",
                 shiny::div(class = "ui header", "Violated data (sample)"),
                 shiny:: HTML(knitr::kable(utils::head(error$error_df[[1]][[.x]]), "html", align = NULL, table.attr = "class=\"ui cellable table\"")),
                 shiny::div(class = "ui horizontal divider", shiny.semantic::uiicon("flag"))
      )
    shiny::tagList(
      shiny::tags$table(class = "ui definition table",
                        shiny::tags$tbody(
                          shiny::tags$tr(
                            shiny::tags$td(shiny::tags$h6("Error message")),
                            shiny::tags$td(class = "middle aligned", shiny::tags$code(style="white-space: pre-wrap; font-size: 0.85em", error$message[[1]][.x]))
                          ),
                          shiny::tags$tr(
                            shiny::tags$td(shiny::tags$h6("Violations")),
                            shiny::tags$td(class = "middle aligned", error$num.violations[[1]][.x])
                          )
                        )
      ),
      data_part
    )
  }) %>% htmltools::tagList()
}

#' Create table row.
#' @description Create table row.
#' @param results Results to display in a row.
#' @param type Result type.
#' @param mark Icon to display.
#' @return Table row.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
make_table_row <- function(results, type, mark) {
  description <- results %>% dplyr::pull(.data$description)
  id <- results %>% dplyr::pull(.data$assertion.id)
  data_name <- results %>% dplyr::pull(.data$table_name)
  result <- type
  button_class <- "ui disabled button"
  onclick <- NULL
  modal <- NULL
  if (result != success_id) {
    button_class <- "ui tiny button"

    onclick <- sprintf("$('#%s').modal('show');", id)
    modal_header <- paste0(data_name, ": ", description)
    modal <-
      shiny::div(id = id, class = "ui longer test modal visible scrolling",
                 shiny::div(class = "header", shiny::tags$h5(modal_header)),
                 shiny::div(class = "scrolling content",
                            shiny::div(class = "description",
                                       shiny::tagList(
                                         prepare_modal_content(results)
                                       )
                            )
                 )
      )
  }

  shiny::tags$tr(
    shiny::tags$td(class = "left aligned",
                   shiny::tags$h6(description)
    ),
    shiny::tags$td(class = "center aligned",
                   shiny.semantic::uiicon(mark)
    ),
    shiny::tags$td(class = "center aligned",
                   shiny::tagList(
                     modal,
                     shiny::tags$button(class = button_class, "Show", onclick = onclick)
                   )
    )
  )
}

#' Create table with results.
#' @description Create table with results.
#' @param results Result to display in table.
#' @param type Result type.
#' @param mark Icon to display.
#' @return Table row.
result_table <- function(results, type, mark) {
  if (nrow(results) == 0) {
    "No cases to display."
  } else {
    results <- dplyr::group_by(results, .data$table_name, .data$assertion.id, .data$description) %>%
      dplyr::summarise_at(dplyr::vars(-dplyr::group_cols()), list)
    shiny::tags$table(class = "ui padded striped table",
                      shiny::tags$thead(
                        shiny::tags$tr(
                          shiny::tags$th(class = "twelve wide left aligned", shiny::tags$h5("Validation rule")),
                          shiny::tags$th(class = "one wide center aligned", shiny::tags$h5("Status")),
                          shiny::tags$th(class = "three wide center aligned", shiny::tags$h5("Error details"))
                        )
                      ),
                      shiny::tags$tbody(
                        purrr::map(1:nrow(results), ~ make_table_row(results[.x, ], type, mark))
                      )
    )
  }
}

#' Create a UI accordion container.
#' @description Create a UI accordion container.
#' @param ... Additional agruments inside accordion container.
#' @return Accordion container.
make_accordion_container <- function(...) {
  shiny::tagList(
    shiny::div(class = "ui styled accordion", style = "width:100%", ...)
  )

}

#' Create a UI accordion element.
#' @description Create a UI accordion element.
#' @param results Results to display.
#' @param mark Icon to display.
#' @param label Label.
#' @param color Color of the label icon.
#' @param type Result type.
#' @param active Is active?
#' @return Accordion.
make_accordion_element <- function(results, color = "green", label, active = FALSE, type, mark) {
  state <- NULL
  if (active) {
    state <- "active"
  }
  shiny::tagList(
    shiny::div(class = paste("title", state), shiny.semantic::uiicon("dropdown"),
               shiny::tagList(shiny.semantic::uilabel(length(unique(results$assertion.id)), type = paste(color, "circular tiny")), label)),
    shiny::div(class = paste("content", state), result_table(results, type, mark))
  )
}

#' Displays results of validations.
#' @description Displays results of validations.
#' @param data Report data.
#' @param n_passed Number of successful assertions.
#' @param n_failed Number of warning assertions.
#' @param n_warned Number of violation assertions.
#' @return Validation report.
display_results <- function(data, n_passed, n_failed, n_warned) {
  data_name <- data$table_name[1]
  results_failed <- data %>%
    dplyr::filter(.data$type == error_id)
  results_warning <- data %>%
    dplyr::filter(.data$type == warning_id)
  results_passed <- data %>%
    dplyr::filter(.data$type == success_id)

  is_negative_active <- is_neutral_active <- FALSE
  label_color_negative <- label_color_neutral <- ""
  if (nrow(results_failed) > 0) {
    is_negative_active <- TRUE
    label_color_negative <- "red"
  }
  if (nrow(results_warning) > 0) {
    is_neutral_active <- TRUE
    label_color_neutral <- "blue"
  }
  segment_title <- data_name
  code <- segment(
    segment_title,
    shiny::p(),
    make_accordion_container(
      if (!is.null(n_failed)) make_accordion_element(
        results = results_failed,
        color = label_color_negative,
        label = "Failed",
        mark = "red big remove",
        type = error_id,
        active = is_negative_active),
      if (!is.null(n_warned)) make_accordion_element(
        results = results_warning,
        color = label_color_neutral,
        label = "Warnings",
        mark = "big blue exclamation circle",
        type = warning_id,
        active = is_neutral_active),
      if (!is.null(n_passed)) make_accordion_element(
        results = results_passed,
        label = "Passed",
        type = success_id,
        mark = "big green checkmark")
    )
  )
  code
}

#' Create summary table row.
#' @description Create summary table row.
#' @param id ID.
#' @param number Number to display.
#' @param color Color of the label.
#' @param label Label to display.
#' @return Summary table row.
create_summary_row <- function(id, number, color, label) {
  list(shiny::tags$td(id = id,
                 class = "two wide right aligned",
                 shiny.semantic::uilabel(number, type = paste(color, "circular huge"))),
  shiny::tags$td(class = "three wide left aligned", shiny::tags$h2(label)))
}

#' Create summary table.
#' @description Create summary table.
#' @param n_passes Nr of passed validations.
#' @param n_fails Nr of failed validations.
#' @param n_warns Nr of warnings.
#' @return Summary table.
make_summary_table <- function(n_passes, n_fails, n_warns) {
  fails_label_color <- "red"
  if (n_fails == 0) {
    fails_label_color <- ""
  }
  warns_label_color <- "blue"
  if (n_warns == 0) {
    warns_label_color <- ""
  }
  code <- shiny::tags$table(id = "summary", class = "ui padded table",
                            shiny::tags$tbody(
                              shiny::tags$tr(
                                if (!is.null(n_fails)) create_summary_row("failed_total", n_fails, fails_label_color, "Failed"),
                                if (!is.null(n_warns)) create_summary_row("warned_total", n_warns, warns_label_color, "Warnings"),
                                if (!is.null(n_passes)) create_summary_row("passed_total", n_passes, "green", "Passed")
                              )
                            )
  )
  code
}

#' Generate HTML report.
#' @description Generate HTML validation report.
#' @param n_passed Number of passed validations
#' @param n_failed Number of failed validations.
#' @param n_warned Number of warnings.
#' @param validation_results Data frame with validation results.
#' @return HTML validation report.
get_semantic_report_ui <- function(n_passed, n_failed, n_warned, validation_results) {
  summary_table <- make_summary_table(n_passed, n_failed, n_warned)
  unique_objects <- validation_results %>% dplyr::pull(.data$table_name) %>% unique()
  html_report <- unique_objects %>% purrr::map(~ {
    validation_results %>% dplyr::filter(.data$table_name == .x) %>%
      display_results(n_passed, n_failed, n_warned)
  }) %>% htmltools::div()
  htmltools::div(summary_table, html_report)
}

post_render_js <- "
  function activateAccordion() {
    $('.ui.accordion').accordion();
  }
  $(window).on('load', function () {
    activateAccordion();
  });
"

#' Render semantic version of report
#'
#' @description Renders content of semantic report version.
#'
#' @param n_passed Number of successful assertions.
#' @param n_failed Number of warning assertions.
#' @param n_warned Number of violation assertions.
#' @param validation_results Validation results table (see \link{get_results}).
#' @export
render_semantic_report_ui <- function(n_passed, n_failed, n_warned, validation_results) {
  get_semantic_report_ui(n_passed, n_failed, n_warned,
                       validation_results) %>%
    shiny.semantic::uirender(width = "100%", height = "100%") %>%
    htmltools::tagList(
      htmlwidgets::onStaticRenderComplete(post_render_js)
    )
}

#' Render simple version of report
#'
#' @description Renders content of simple report version that prints \code{validation_results} table.
#'
#' @param n_passed Number of successful assertions.
#' @param n_failed Number of warning assertions.
#' @param n_warned Number of violation assertions.
#' @param validation_results Validation results table (see \link{get_results}).
#' @export
render_raw_report_ui <- function(n_passed, n_failed, n_warned, validation_results) {
  types <- c(
    if(!is.null(n_passed)) success_id,
    if(!is.null(n_warned)) warning_id,
    if(!is.null(n_failed)) error_id
  )
  shiny::tagList(
    if(!is.null(n_passed)) shiny::tags$div("Passed rules:", n_passed),
    if(!is.null(n_failed)) shiny::tags$div("Failed rules:", n_failed),
    if(!is.null(n_warned)) shiny::tags$div("Warning rules:", n_warned),
    shiny:: HTML(
      knitr::kable(
        tidyr::unnest(validation_results, .data$error_df, keep_empty = TRUE) %>%
          dplyr::filter(.data$type %in% types),
        "html", align = NULL, table.attr = "class=\"ui cellable table\""
      )
    )
  )
}
