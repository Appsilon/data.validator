datavalidator_constants <- new.env()
datavalidator_constants$n_failed <- 0
datavalidator_constants$n_passed <- 0
datavalidator_constants$n_warned <- 0

#' Report constants.
#' @description Report constants.
#' @return Report constants.
#' @export
datavalidator_constants <- datavalidator_constants

#' Generate a random ID.
#' @description Generate a random ID.
#' @return A characters corresponding to random ID.
generate_id <- function() {
  paste0(sample(c(LETTERS, letters, 0:9), 20, TRUE), collapse = "")
}

#' Create a UI segment element.
#' @description Create a UI segment element.
#' @param title Title of the segment.
#' @param ... Additional agruments inside sagment.
#' @param color Color of the segment.
#' @return Segment.
segment <- function(title, ..., color = "blue") {
  shiny::div(class = "ui raised segment", style = "margin-bottom: 0.5em",
      shiny::div(class = paste("ui demo ribbon label", color), title),
      ...
  )
}

#' Prepare modal content.
#' @description Prepare modal content.
#' @param error Error.
#' @return Modal content.
prepare_modal_content <- function(error) {
  data_part <- NULL
  if (!is.null(error$error_df)) {
    data_part <-
      shiny::div(style = "padding-left: 1em;",
          shiny::div(class = "ui header", "Violated data"),
          shiny:: HTML(knitr::kable(utils::head(error$error_df), "html", align = NULL, table.attr = "class=\"ui cellable table\"")),
          shiny::div(class = "ui horizontal divider", shiny.semantic::uiicon("flag"))
      )
  }
  shiny::tagList(
    shiny::tags$table(class = "ui definition table",
               shiny::tags$tbody(
                 shiny::tags$tr(
                   shiny::tags$td(shiny::tags$h6("Error message")),
                   shiny::tags$td(class = "middle aligned", shiny::tags$code(style="white-space: pre-wrap; font-size: 0.85em", error$message))
                 ),
                 shiny::tags$tr(
                   shiny::tags$td(shiny::tags$h6("Violations")),
                   shiny::tags$td(class = "middle aligned", error$num.violations)
                 )
               )
    ),
    data_part
  )
}

#' Create table row.
#' @description Create table row.
#' @param title Row title.
#' @param result Result to display in row.
#' @param id Row id.
#' @param data_name Data name.
#' @param errors Errors.
#' @param mark Mark.
#' @return Table row.
#' @importFrom dplyr %>%
make_table_row <- function(title, result, id, data_name, errors, mark) {
  button_class <- "ui disabled button"
  onclick <- NULL
  modal <- NULL
  if (!result) {
    error_position <- errors %>% purrr::map_lgl(~ .$validation_id == id) %>% which()
    current_errors <- errors[error_position]
    button_class <- "ui tiny button"

    onclick <- sprintf("$('#%s').modal('show');", id)
    modal_header <- paste0(data_name, ": ", title)
    modal <-
      shiny::div(id = id, class = "ui longer test modal visible scrolling",
          shiny::div(class = "header", shiny::tags$h5(modal_header)),
          shiny::div(class = "scrolling content",
              shiny::div(class = "description",
                  shiny::tagList(
                    purrr::map(current_errors, ~ prepare_modal_content(.x))
                  )
              )
          )
      )
  }

  shiny::tags$tr(
    shiny::tags$td(class = "left aligned",
            shiny::tags$h6(title)
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
#' @param data_name Data name.
#' @param errors Errors.
#' @param mark Mark.
#' @return Table row.
result_table <- function(results, errors, data_name, mark) {
  if (length(results) == 0) {
    "No cases to display."
  } else {
    shiny::tags$table(class = "ui padded striped table",
               shiny::tags$thead(
                 shiny::tags$tr(
                   shiny::tags$th(class = "twelve wide left aligned", shiny::tags$h5("Validation rule")),
                   shiny::tags$th(class = "one wide center aligned", shiny::tags$h5("Status")),
                   shiny::tags$th(class = "three wide center aligned", shiny::tags$h5("Error details"))
                 )
               ),
               shiny::tags$tbody(
                 purrr::map(results, ~ make_table_row(.x$title, .x$result, .x$validation_id, data_name, errors, mark))
               )
    )
  }
}

#' Create a UI accordion container.
#' @description Create a UI accordion container.
#' @param ... Additional agruments inside accordion container.
#' @return Accordion container.
make_accordion_container <- function(...) {
  id <- generate_id()
  shiny::tagList(
    shiny::div(id = id, class = "ui styled accordion", style = "width:100%", ...),
    shiny::tags$script(sprintf("$('#%s').accordion();", id))
  )

}

#' Create a UI accordion element.
#' @description Create a UI accordion element.
#' @param title Title of the accordion.
#' @param content Content of the accordion.
#' @param active Is active?
#' @return Accordion.
make_accordion_element <- function(title, content, active = FALSE) {
  state <- NULL
  if (active) {
    state <- "active"
  }
  shiny::tagList(
    shiny::div(class = paste("title", state), shiny.semantic::uiicon("dropdown"), onclick = "classToggle(this)", title),
    shiny::div(class = paste("content", state), content)
  )
}

## Changed 9 lines 1 arg
#' Displays results of validations.
#' @description Displays results of validations.
#' @param data Report data.
#' @param make_report Make report.
#' @param repo_path Github repo path.
#' @return Validation report.
#' @export
display_results <- function(data, make_report, repo_path = NULL) {
  if (make_report) {
    errors <- attr(data, "assertr_errors")
    warnings <- attr(data, "assertr_warnings")
    results <- attr(data, "assertr_results")
    results_ids <- results %>% purrr::map_chr(~ .$validation_id)
    warnings_ids <- warnings %>% purrr::map_chr(~ .$validation_id)
    errors_ids <- errors %>% purrr::map_chr(~ .$validation_id)
    results_negative_idx <- which(results_ids %in% errors_ids)
    results_neutral_idx <- which(results_ids %in% warnings_ids)
    results_positive_idx <- base::setdiff(1:length(results), c(results_negative_idx, results_neutral_idx))
    assign("n_failed", get("n_failed", datavalidator_constants) + length(results_negative_idx), envir = datavalidator_constants)
    assign("n_warned", get("n_warned", datavalidator_constants) + length(results_neutral_idx), envir = datavalidator_constants)
    assign("n_passed", get("n_passed", datavalidator_constants) + length(results_positive_idx), envir = datavalidator_constants)
    is_negative_active <- is_neutral_active <- FALSE
    label_color_negative <- label_color_neutral <- ""
    if (length(results_negative_idx) > 0) {
      is_negative_active <- TRUE
      label_color_negative <- "red"
    }
    if (length(results_neutral_idx) > 0) {
      is_neutral_active <- TRUE
      label_color_neutral <- "blue"
    }
    if (!is.null(repo_path)) {
      line_number <- attr(data, "line_number")
      github_link <- if (is.null(line_number) || line_number == "") {
        NULL
      } else {
        shiny::tags$a(shiny.semantic::uiicon("github square white large"), href = return_repo_link(repo_path, line_number), target = "_blank", style = "opacity:1; margin-left:1em;")
      }
    }
    data_name <- ifelse(is.null(attr(data, "ribbon-title")), deparse(substitute(data)), attr(data, "ribbon-title"))
    first_taglist <- if (!is.null(repo_path)) {
      shiny::tagList(
        data_name,
        github_link
      )
    } else {
      data_name
    }
    code <- segment(
      first_taglist,
      shiny::p(),
      make_accordion_container(
        make_accordion_element(
          title = shiny::tagList(shiny.semantic::uilabel(length(results_negative_idx), type = paste(label_color_negative, "circular tiny")), "Failed"),
          content = result_table(results[results_negative_idx], errors, data_name, "red big remove"),
          active = is_negative_active),
        make_accordion_element(
          title = shiny::tagList(shiny.semantic::uilabel(length(results_neutral_idx), type = paste(label_color_neutral, "circular tiny")), "Warnings"),
          content = result_table(results[results_neutral_idx], warnings, data_name, "big blue exclamation circle"),
          active = is_neutral_active),
        make_accordion_element(
          title = shiny::tagList(shiny.semantic::uilabel(length(results_positive_idx), type = "green circular tiny"), "Passed"),
          content = result_table(results[results_positive_idx], errors, data_name, "big green checkmark"))
      )
    )
    shiny.semantic::uirender(code, width = "100%", height = "100%")
  } else {
    invisible(data)
  }
}

#' Return line number.
#' @description Return line number.
#' @param identifier Identifier.
#' @param file_path File path.
#' @param offset Offset.
#' @return Line number.
return_line_number <- function(identifier, file_path, offset = -1) {
  if (!is.null(file_path) && file_path != "") {
    line_number <- grep(identifier, readLines(file_path))[1]
    if (length(line_number) == 0) {
      ""
    } else {
      as.character(line_number + offset)
    }
  } else {
    ""
  }
}

## Only Kesko
#' Return repo link.
#' @description Return repo link.
#' @param line_number Line number.
#' @param repo_path Github repo path.
#' @return Repo link.
return_repo_link <- function(repo_path, line_number) {
  sprintf("%s#L%s", repo_path, line_number)
}

#' Assign data line number.
#' @description Assign data line number.
#' @param data Data.
#' @param identifier Identifier.
#' @param file_path File path.
#' @return Line number assigned.
#' @export
assign_data_line_number <- function(data, identifier, file_path) {
  if (file_path != "") {
    attr(data, "line_number") <- return_line_number(identifier, file_path)
  } else {
    attr(data, "line_number") <- ""
  }
  return(data)
}

#' Create summary table.
#' @description Create summary table.
#' @param n_passes Nr of passed validations
#' @param n_fails Nr of failed validations.
#' @param n_warns Nr of warnings.
#' @return Summary table.
#' @export
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
                         shiny::tags$td(id = "failed_total",
                                 class = "two wide right aligned",
                                 shiny.semantic::uilabel(n_fails, type = paste(fails_label_color, "circular huge"))),
                         shiny::tags$td(class = "three wide left aligned", shiny::tags$h2("Failed")),
                         shiny::tags$td(id = "warned_total",
                                 class = "two wide right aligned",
                                 shiny.semantic::uilabel(n_warns, type = paste(warns_label_color, "circular huge"))),
                         shiny::tags$td(class = "three wide left aligned", shiny::tags$h2("Warnings")),
                         shiny::tags$td(id = "passed_total",
                                 class = "two wide right aligned",
                                 shiny.semantic::uilabel(n_passes, type = "circular huge green")),
                         shiny::tags$td(class = "three wide left aligned", shiny::tags$h2("Passed"))
                       )
                     )
  )
  shiny.semantic::uirender(code, width = "100%", height = "100%")
}
