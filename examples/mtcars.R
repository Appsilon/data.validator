library(dplyr)
library(assertr)
library(datavalidator)
validator <- Validator$new()

validator <- create_validator()
mtcars %>%
  chain_start(store_success = TRUE) %>%
  assert(description = "mpg:carb shouldn't have missing values", not_na, mpg:carb) %>%
  assert(description = "vs and am shuld be in 0, 2", in_set(c(0, 2)), vs, am) %>%
  assert(description = "gear and carb shuld be in 3, 4", skip_chain_opts = TRUE,
         error_fun = warning_append, in_set(c(3, 4)), gear, carb) %>%
  assert_rows(description = "rowsSums for am:vs should be less than 2", rowSums, within_bounds(0, 2), vs:am) %>%
  assert_rows(description = "rowsSums for am:vs should be less than 1", rowSums, within_bounds(0, 1), vs:am) %>%
  insist(description = "wt and qsec variable should fit in 4 times own sds", within_n_sds(4), wt, qsec) %>%
  insist(description = "wt and qsec variable should fit in 2 times own sds", within_n_sds(2), wt, qsec) %>%
  insist_rows(description = "mpg:carb observations maha_dist shoueld be in 30 times own sds",
              maha_dist, within_n_mads(30), mpg:carb) %>%
  insist_rows(description = "mpg:carb observations maha_dist shoueld be in 3 times own sds",
              maha_dist, within_n_mads(3), mpg:carb) %>%
  verify(description = "drat should be positive", drat > 0) %>%
  verify(description = "drat should have values over 3", drat > 3) %>%
  chain_end(error_fun = error_append) %>%
  add_validation()

validator$get_validations()

validator$save_html_report(summary = c("warning", "error", "success"))

validator$save_html_report(summary = c("warning", "error"))

validator$save_html_report(
  system.file("rmarkdown/templates/raw/skeleton/skeleton.Rmd", package = "datavalidator"),
  summary = c("warning", "error", "success"),
  render_report_ui = render_raw_report_ui
)

validator$save_log("results", type = "csv")
