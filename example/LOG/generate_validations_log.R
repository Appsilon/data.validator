library(datavalidator)
library(assertr)
library(dplyr)
# Create new 'validator' object of class 'Validator'
validator <- Validator$new()

# We can set url to Github repo by:
validator$repo_path <- "https://github.com/Appsilon/datavalidator"

# Let's add some basic validations:
sample_data <- tibble(
  x = letters[1:3],
  y = letters[1:3],
  z = 1:3) %>%
  clear_results() %>%  chain_start() %>%
  verify(title = "x should have character class", v_class(x) == "character") %>%
  verify(title = "y should have numeric class", v_class(y) == "numeric") %>%
  verify(title = "y should have date class", v_class(y) == "Date") %>%
  verify(title = "z should have Date class", ignore_chain_funs = TRUE,
         error_fun = append_as_warning, v_class(z) == "Date") %>%
  chain_end(error_fun = error_append)

validator$add_validations(sample_data)

# If we want to do some action depending on validation results we can use `get_validations()` method
if (validator$get_validations()$n_failed > 0) print("We've got a problem!")

# To save report log we have to call `generate_report_log()` method
validator$save_log(output_path = "example/LOG/validation_log")
