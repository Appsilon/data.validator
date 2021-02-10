library(readr)
library(data.validator)
library(dplyr)

# Data comes from tidy tuesday
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

report <- data_validation_report()
validate(beer_states) %>%
  assert_cols(description = "No NA's inside barrels column", predicate = assertr::not_na, barrels) %>%
  assert_if(description = "Check if year is in correct range (2008, 2018)", year >= 2008 && year <= 2018) %>%
  add_results(report)

save_report(report, success = FALSE)
browseURL("validation_report.html")
