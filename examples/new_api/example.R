library(dplyr)
library(assertr)
library(data.validator)

report <- data_validation_report()

validate(mtcars, description = "Motor Trend Car Road Tests") %>%
  assert_cols(description = "No NA's inside mpg:carb columns", predicate = not_na, mpg:carb) %>%
  assert_cols(description = "vs and am values equal 0 or 2 only", predicate = in_set(c(0, 2)), vs, am) %>%
  assert_cols(description = "gear and carb values should equal 3 or 4", skip_chain_opts = TRUE,
              error_fun = warning_append, predicate = in_set(c(3, 4)), gear, carb) %>%
  assert_rows(description = "Each row sum for am:vs columns is less or equal 2",
              row_reduction_fn = rowSums, predicate = within_bounds(0, 2), vs:am) %>%
  assert_rows(description = "Each row sum for am:vs columns is less or equal 1",
              row_reduction_fn = rowSums, predicate = within_bounds(0, 1), vs:am) %>%
  assert_cols(description = "For wt and qsec we have: abs(col) < 4 * sd(col)",
              predicate_generator = within_n_sds(4), wt, qsec) %>%
  assert_cols(description = "For wt and qsec we have: abs(col) < 2 * sd(col)",
              predicate_generator = within_n_sds(2), wt, qsec) %>%
  assert_rows(description = "Using mpg:carb mahalanobis distance for each observation is within 30 median absolute deviations from the median",
              row_reduction_fn = maha_dist, predicate_generator = within_n_mads(30), mpg:carb) %>%
  assert_rows(description = "Using mpg:carb mahalanobis distance for each observation is within 3 median absolute deviations from the median",
              row_reduction_fn = maha_dist, predicate_generator = within_n_mads(3), mpg:carb) %>%
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