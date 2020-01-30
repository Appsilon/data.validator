#' @export
dataval_deps <- htmltools::htmlDependency(
  "datavalidator",
  "1.0",
  src = system.file('rmarkdown', package = 'datavalidator'),
  script = c("templates/report/skeleton/report.js")
)
