#' @export
semantic_report_deps <- function() {
  htmltools::htmlDependency(
    "datavalidator",
    "1.0",
    src = system.file('www', package = 'datavalidator'),
    script = c("report.js")
  )
}
