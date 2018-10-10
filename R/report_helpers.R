#' Generate a random ID.
#' @description Generate a random ID.
#' @return A characters corresponding to random ID.
#' @export
generate_id <- function() {
  paste0(sample(c(LETTERS, letters, 0:9), 20, TRUE), collapse = "")
}

#' Create a UI segment element.
#' @description Create a UI segment element.
#' @param title Title of the segment.
#' @param ... Additional agruments inside sagment.
#' @param color Color of the segment.
#' @return Segment.
#' @export
segment <- function(title, ..., color = "blue") {
  div(class = "ui raised segment", style = "margin-bottom: 0.5em",
      tags$div(class = paste("ui demo ribbon label", color), title),
      ...
  )
}

prepare_modal_content <- function(error) {
  data_part <- NULL
  if (!is.null(error$error_df)) {
    data_part <-
      div(style = "padding-left: 1em;",
          div(class = "ui header", "Violated data"),
          HTML(knitr::kable(head(error$error_df), "html", align = NULL, table.attr = "class=\"ui cellable table\"")),
          div(class = "ui horizontal divider", uiicon("flag"))
      )
  }
  tagList(
    tags$table(class = "ui definition table",
               tags$tbody(
                 tags$tr(
                   tags$td(tags$h6("Error message")),
                   tags$td(class = "middle aligned", tags$code(style="white-space: pre-wrap; font-size: 0.85em", error$message))
                 ),
                 tags$tr(
                   tags$td(tags$h6("Violations")),
                   tags$td(class = "middle aligned", error$num.violations)
                 )
               )
    ),
    data_part
  )
}

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
      div(id = id, class = "ui longer test modal visible scrolling",
          div(class = "header", tags$h5(modal_header)),
          div(class = "scrolling content",
              div(class = "description",
                  tagList(
                    purrr::map(current_errors, ~ prepare_modal_content(.x))
                  )
              )
          )
      )
  }

  tags$tr(
    tags$td(class = "left aligned",
            tags$h6(title)
    ),
    tags$td(class = "center aligned",
            shiny.semantic::uiicon(mark)
    ),
    tags$td(class = "center aligned",
            tagList(
              modal,
              tags$button(class = button_class, "Show", onclick = onclick)
            )
    )
  )
}

result_table <- function(results, errors, data_name, mark) {
  if (length(results) == 0) {
    "No cases to display."
  } else {
    tags$table(class = "ui padded striped table",
               tags$thead(
                 tags$tr(
                   tags$th(class = "twelve wide left aligned", tags$h5("Validation rule")),
                   tags$th(class = "one wide center aligned", tags$h5("Status")),
                   tags$th(class = "three wide center aligned", tags$h5("Error details"))
                 )
               ),
               tags$tbody(
                 purrr::map(results, ~ make_table_row(.x$title, .x$result, .x$validation_id, data_name, errors, mark))
               )
    )
  }
}

make_accordion_container <- function(...) {
  id <- generate_id()
  tagList(
    tags$div(id = id, class = "ui styled accordion", style = "width:100%", ...),
    tags$script(sprintf("$('#%s').accordion();", id))
  )

}

make_accordion_element <- function(title, content, active = FALSE) {
  state <- NULL
  if (active) {
    state <- "active"
  }
  tagList(
    div(class = paste("title", state), uiicon("dropdown"), onclick = "classToggle(this)", title),
    div(class = paste("content", state), content)
  )
}

## Changed 9 lines 1 arg
display_results <- function(data, make_report, repo_path) {
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
    n_failed <<- n_failed + length(results_negative_idx)
    n_warned <<- n_warned + length(results_neutral_idx)
    n_passed <<- n_passed + length(results_positive_idx)
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
    line_number <- attr(data, "line_number")
    github_link <- if (is.null(line_number) || line_number == "") {
      NULL
    } else {
      tags$a(uiicon("github square white large"), href = return_repo_link(repo_path, line_number), target = "_blank", style = "opacity:1; margin-left:1em;")
    }
    data_name <- ifelse(is.null(attr(data, "ribbon-title")), deparse(substitute(data)), attr(data, "ribbon-title"))
    code <- segment(
      tagList(
        data_name,
        github_link
      ),
      p(),
      make_accordion_container(
        make_accordion_element(
          title = tagList(uilabel(length(results_negative_idx), type = paste(label_color_negative, "circular tiny")), "Failed"),
          content = result_table(results[results_negative_idx], errors, data_name, "red big remove"),
          active = is_negative_active),
        make_accordion_element(
          title = tagList(uilabel(length(results_neutral_idx), type = paste(label_color_neutral, "circular tiny")), "Warnings"),
          content = result_table(results[results_neutral_idx], warnings, data_name, "big blue exclamation circle"),
          active = is_neutral_active),
        make_accordion_element(
          title = tagList(uilabel(length(results_positive_idx), type = "green circular tiny"), "Passed"),
          content = result_table(results[results_positive_idx], errors, data_name, "big green checkmark"))
      )
    )
    shiny.semantic::uirender(code, width = "100%", height = "100%")
  } else {
    invisible(data)
  }
}

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
return_repo_link <- function(repo_path, line_number) {
  sprintf("%s#L%s", repo_path, line_number)
}

assign_data_line_number <- function(data, identifier, file_path) {
  if (file_path != "") {
    attr(data, "line_number") <- return_line_number(identifier, file_path)
  } else {
    attr(data, "line_number") <- ""
  }
  return(data)
}

make_summary_table <- function(n_passes, n_fails, n_warns) {
  fails_label_color <- "red"
  if (n_fails == 0) {
    fails_label_color <- ""
  }
  warns_label_color <- "blue"
  if (n_warns == 0) {
    warns_label_color <- ""
  }
  code <- tags$table(id = "summary", class = "ui padded table",
                     tags$tbody(
                       tags$tr(
                         tags$td(id = "failed_total",
                                 class = "two wide right aligned",
                                 uilabel(n_fails, type = paste(fails_label_color, "circular huge"))),
                         tags$td(class = "three wide left aligned", tags$h2("Failed")),
                         tags$td(id = "warned_total",
                                 class = "two wide right aligned",
                                 uilabel(n_warned, type = paste(warns_label_color, "circular huge"))),
                         tags$td(class = "three wide left aligned", tags$h2("Warnings")),
                         tags$td(id = "passed_total",
                                 class = "two wide right aligned",
                                 uilabel(n_passes, type = "circular huge green")),
                         tags$td(class = "three wide left aligned", tags$h2("Passed"))
                       )
                     )
  )
  shiny.semantic::uirender(code, width = "100%", height = "100%")
}

repo_path <- "https://github.com/kesko-dev/onninen-data-validation/tree/master"
make_report = getOption("display_report_summary", default = FALSE)
validation_script_path <- if (exists("validation_script_path")) validation_script_path else ""
preprocessing_script_path <- if (exists("preprocessing_script_path")) preprocessing_script_path else ""
gh_repo_validation_script_path <- file.path(repo_path, gsub("onninen-data-validation/", "", validation_script_path))
n_failed <- 0
n_passed <- 0
n_warned <- 0
