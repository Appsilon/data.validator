# Description

`datavalidator` is a set of tools for creating a HTML validation report.

# How to use it

1. Create a new validation report template or modify existing `Rmd` file.

To create new template opne `File -> New file -> R Markdown...` and select `Appsilon Validation Report Template (datavalidator)` form `From template` tab.

If you are working with existing report and you want to use `datavalidator` package you need to change a few things inside your `Rmd` file:
 - add `library(datavalidator)` (instead of `source(report_helpers.R)`)
 - use variables from hidden `datavalidator_constants` environment e.g. `datavalidator_constants$n_failed` instead of `n_failed`, `datavalidator_constants$n_passed` instead of `n_passed` and `datavalidator_constants$n_warned` insted of `n_warned`.
  - if needed declare `make_report = getOption("display_report_summary", default = FALSE)`, `repo_path` and `gh_repo_validation_script_path`

# To build package in docker use:

```
devtools::build(path = "/mnt/package")
```
