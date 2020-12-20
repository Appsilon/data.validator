library(dplyr)
library(assertr)
library(data.validator)

report <- data_validation_report()

validate(mtcars, description = "Motor Trend Car Road Tests") %>%
  assert_cols(description = "No NA's inside mpg:carb columns", not_na, mpg:carb) %>%
  assert_cols(description = "vs and am values equal 0 or 2 only", in_set(c(0, 2)), vs, am) %>%
  # assert_cols(description = "vs and am values should equal 3 or 4", skip_chain_opts = TRUE,
  #       error_fun = warning_append, in_set(c(3, 4)), gear, carb) %>%
  # TODO: we should have different API for "warning" assertions
  assert_rows(description = "Each row sum for am:vs columns is less or equal 2",
              rowSums, within_bounds(0, 2), vs:am) %>%
  assert_rows(description = "Each row sum for am:vs columns is less or equal 1",
              rowSums, within_bounds(0, 1), vs:am) %>%
  assert_cols(description = "For wt and qsec we have: abs(col) < 4 * sd(col)",
              within_n_sds(4), wt, qsec, predicate_calculated_from_column = TRUE) %>%
  assert_cols(description = "For wt and qsec we have: abs(col) < 2 * sd(col)",
              within_n_sds(2), wt, qsec, predicate_calculated_from_column = TRUE) %>%
  assert_rows(description = "Using mpg:carb mahalanobis distance for each observation is within 30 median absolute deviations from the median",
              maha_dist, within_n_mads(30), mpg:carb, predicate_calculated_from_reduced_row = TRUE) %>%
  assert_rows(description = "Using mpg:carb mahalanobis distance for each observation is within 3 median absolute deviations from the median",
              maha_dist, within_n_mads(3), mpg:carb, predicate_calculated_from_reduced_row = TRUE) %>%
  assert_if(description = "Column drat has only positive values", drat > 0) %>%
  assert_if(description = "Column drat has only values larger than 3", drat > 3) %>%
  add_results(report)

get_results(report)

save_report(report)
browseURL("validation_report.html")

save_report(report, success = FALSE)
browseURL("validation_report.html")

save_report(report, ui_constructor = render_raw_report_ui)
browseURL("validation_report.html")

save_results(report, "results.csv")

save_summary(report, "validation_log.txt")
