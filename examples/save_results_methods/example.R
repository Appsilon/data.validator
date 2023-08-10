library(dplyr)
library(assertr)
library(data.validator)

validator <- data_validation_report()
validate(mtcars) %>%
  validate_cols(description = "No NA's inside mpg:carb columns", not_na, mpg:carb) %>%
  validate_cols(description = "vs and am values equal 0 or 2 only", in_set(c(0, 2)), vs, am) %>%
  validate_cols(description = "vs and am values should equal 3 or 4", skip_chain_opts = TRUE,
                error_fun = warning_append, in_set(c(3, 4)), gear, carb) %>%
  validate_rows(description = "Each row sum for am:vs columns is less or equal 2",
                rowSums, within_bounds(0, 2), vs:am) %>%
  validate_rows(description = "Each row sum for am:vs columns is less or equal 1",
                rowSums, within_bounds(0, 1), vs:am) %>%
  validate_cols(description = "For wt and qsec we have: abs(col) < 4 * sd(col)", within_n_sds(4), wt, qsec) %>%
  validate_cols(description = "For wt and qsec we have: abs(col) < 2 * sd(col)", within_n_sds(2), wt, qsec) %>%
  validate_rows(description = "Using mpg:carb mahalanobis distance for each observation is within 30 median absolute deviations from the median",
                row_reduction_fn = maha_dist, within_n_mads(30), mpg:carb) %>%
  validate_rows(description = "Using mpg:carb mahalanobis distance for each observation is within 3 median absolute deviations from the median",
                row_reduction_fn = maha_dist, within_n_mads(3), mpg:carb) %>%
  validate_if(description = "Column drat has only positive values", drat > 0) %>%
  validate_if(description = "Column drat has only values larger than 3", drat > 3) %>%
  add_results(validator)

get_results(validator)

# We create a wrapper function for saveRDS.
# Pass this function as method in `save_results`.
write_rds <- function(x, file, ...) {
  saveRDS(object = x, file = file, ...)
}
save_results(validator, file_name = "results.rds", method = write_rds)

# We create a wrapper function for save from base R.
# Pass this function as method in `save_results`.
base_save <- function(x, file, ...) {
  save(x, file = file, ...)
}
save_results(validator, file_name = "results", method = base_save)

