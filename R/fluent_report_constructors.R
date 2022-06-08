library(shiny.fluent)
library(shiny)

Card <- function(title, content) {
  div(class = "card ms-depth-8",
      Stack(
        tokens = list(childrenGap = 5),
        Text(variant = "large", title, block = TRUE),
        content
      )
  )
}

ValidationStatusRow <- function(id, text, success = TRUE, warning = FALSE) {
  config <- list(
    success = list(class = "success", icon = "CheckMark", iconClass = "successIcon"),
    warning = list(class = "warning", icon = "Warning12", iconClass = "warningIcon"),
    failure = list(class = "failure", icon = "Cancel", iconClass = "failureIcon")
  )

  params <- if (success) config$success else if (warning) config$warning else config$failure

  div(className = params$class,
      tagList(
        Separator(),
        div(
          ActionButton(
            paste0("validation_", id),
            iconProps = list("iconName" = params$icon),
            text = text,
            className = params$iconClass
          ),
          if (success) NULL else div(style = "float: right", DefaultButton.shinyInput(
            paste0("open_viewer_button_", id),
            iconProps = list("iconName" = "Search"),
            text = "Inspect invalid data"
          ))
        )
      )
  )
}

ValidationStatusSection <- function(name, description, validations) {
  Card(name,
       tagList(
         Text(variant = "medium", description),
         lapply(validations, function(result) {
           ValidationStatusRow(result$id, result$title, success = result$success, warning = result$warning)
         })
       )
  )
}

layout <- function(body) {
  div(class = "grid-container",
      div(class = "left_margin", ""),
      div(class = "main", body),
      div(class = "right_margin", "")
  )
}


render_fluent_report_ui <- function(validation_results, success = TRUE, warning = TRUE, error = TRUE) {
  tagList(
  div(style = "margin: 20px 0", Text(variant = "xLarge", "Data Validation Report")),
  div(style = "margin: 20px 0",
      span(id = "report_status", style = "float: left; margin-right: 20px;",
           PrimaryButton(input_id = "validation_result", iconProps = list("iconName" = "Cancel"), text = "Failed")
      ),
      span(style = "float: left; padding-top: 5px; margin-right: 5px",
           Toggle("toggle", FALSE,
                  onChanged = JS("(checked) => checked ? $('.success').fadeOut(1000) : $('.success').fadeIn(1000)"))
      ),
      span(style = "float: left; padding-top: 5px",
           Text(variant = "medium", "Display errors and warnings only")
      ),
      span(style="float: right",
           TooltipHost(content = "Date when validation was performed", delay = 0,
                       tagList(FontIcon(iconName = "Clock"), Text(Sys.time())))
      )
  ),
  br(),
  Pivot(
    PivotItem(
      headerText = "Validation Results",
      tagList(
        Separator()#,
        # lapply(validation_results, function(result) {
        #   ValidationStatusSection(result$name, result$description, result$validations)
        # })
      )
    )
  )
)
}
