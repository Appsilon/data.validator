library(readr)
library(data.validator)
library(dplyr)

# Data comes from tidy tuesday
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

report <- data_validation_report()
validate(beer_states) %>%
  validate_cols(description = "No NA's inside barrels column", assertr::not_na, cols = barrels) %>%
  validate_if(description = "Check if year is in correct range (2008, 2018)", year >= 2008 && year <= 2018) %>%
  add_results(report)


# We create a wrapper function for saveRDS.
# `save_results` only supports functions with 'x' and 'file' arguments.
# So, we need a wrapper function to translate the function's arguments
# to a function that has arguments 'x' and 'file'.
# Pass this function as method in `save_results`.
write_rds <- function(x, file, ...) {
  saveRDS(object = x, file = file, ...)
}
save_results(report, file_name = "results.rds", method = write_rds)

# We create a wrapper function for save from base R.
# `save_results` only supports functions with 'x' and 'file' arguments.
# So, we need a wrapper function to translate the function's arguments
# to a function that has arguments 'x' and 'file'.
# Pass this function as method in `save_results`.
base_save <- function(x, file, ...) {
  save(x, file = file, ...)
}
save_results(report, file_name = "results", method = base_save)

