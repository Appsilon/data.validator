library(dplyr)
library(assertr)
library(data.validator)

validator <- create_validator()
mtcars %>%
  chain_start(store_success = TRUE) %>%
  assert(description = "No NA's inside mpg:carb columns", not_na, mpg:carb) %>%
  assert(description = "vs and am values equal 0 or 2 only", in_set(c(0, 2)), vs, am) %>%
  assert(description = "vs and am values should equal 3 or 4", skip_chain_opts = TRUE,
         error_fun = warning_append, in_set(c(3, 4)), gear, carb) %>%
  assert_rows(description = "Each row sum for am:vs columns is less or equal 2", rowSums, within_bounds(0, 2), vs:am) %>%
  assert_rows(description = "Each row sum for am:vs columns is less or equal 1", rowSums, within_bounds(0, 1), vs:am) %>%
  insist(description = "For wt and qsec we have: abs(col) < 4 * sd(col)", within_n_sds(4), wt, qsec) %>%
  insist(description = "For wt and qsec we have: abs(col) < 2 * sd(col)", within_n_sds(2), wt, qsec) %>%
  insist_rows(description = "Using mpg:carb mahalanobis distance for each observation is within 30 median absolute deviations from the median",
              maha_dist, within_n_mads(30), mpg:carb) %>%
  insist_rows(description = "Using mpg:carb mahalanobis distance for each observation is within 3 median absolute deviations from the median",
              maha_dist, within_n_mads(3), mpg:carb) %>%
  verify(description = "Column drat has only positive values", drat > 0) %>%
  verify(description = "Column drat has only values larger than 3", drat > 3) %>%
  chain_end(error_fun = error_append) %>%
  add_results(validator)

get_results(validator)

save_report(validator)
browseURL("validation_report.html")

save_report(validator, success = FALSE)
browseURL("validation_report.html")

save_report(validator, ui_constructor = render_raw_report_ui)
browseURL("validation_report.html")

save_results(validator, "results.csv")

save_summary(validator, "validation_log.txt")
