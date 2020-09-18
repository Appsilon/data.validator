library(readr)
library(assertr)
library(data.validator)
library(dplyr)

# Data comes from tidy tuesday
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

validator <- create_validator()
beer_states %>%
  chain_start(store_success = TRUE) %>%
  assert(description = "No NA's inside barrels column", not_na, barrels) %>%
  verify(description = "Check if year is in correct range (2008, 2018)", year >= 2008 && year <= 2018) %>%
  chain_end(error_fun = error_append) %>%
  add_results(validator)

save_report(validator, summary = c("warning", "error", "success"))
browseURL("validation_report.html")
