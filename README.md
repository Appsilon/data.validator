# Description

`datavalidator` is a set of tools for creating a HTML validation report.

# How to generate HTML report:

1. Create a new validation report template.

To create new template open `File -> New file -> R Markdown...` and select `Appsilon Validation Report Template (datavalidator)` form `From template` tab.

2. The newly created report template has predefined structure. Add `R` scripts to run in the validation process:
```
- VALIDATION_REPORT_FOLDER
|- VALIDATION_REPORT.Rmd
|- logo.png
|- report.js
|- {R_SCRIPTS.R}
```

The `VALIDATION_REPORT.Rmd` file is responsible for HTML report generation and it depends on remaining files.
The `logo.png` file contains logo that will be shown at the top of the generated report.
The `report.js` file contains additional `java script` code for displaying the report correctly.
The `{R_SCRIPTS.R}` are additional files to run inside the report (data reading, validations, e.t.c)

Go to `example/HTML` folder to see the full example.

3. Run `datavalidator::render_validation_report()` function to generate the `HTML` report, for example:

```{r}
datavalidator::render_validation_report(
 template = "example/HTML/example.Rmd", 
 output_dir = "example/HTML/",
 output_file = "validation_report.html",
 repo_path = "https://github.com/Appsilon/datavalidator",
 scripts = c("prepare_data.R", "validation_rules.R"))
```

# How to log validation results without HTML report:

Simply create an `R` validation script and use `generate_report_log()` method.

EXAMPLE:

```{r}
library(datavalidator)
library(assertr)
library(dplyr)
# Create new 'validator' object of class 'Validator'
validator <- Validator$new()

# After initialization number of warnings, failed and passed validations is set to 0
validator
# Github repo path:  NOT DEFINED 
# 
# Validation summary: 
#  Number of fulfilled validations: 0
#  Number of failed validations: 0
#  Number of validations with warnings: 0

# We can set url to Github repo by:
validator$repo_path <- "https://github.com/my_account/my_proj/"

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

validator
# Github repo path:  https://github.com/Appsilon/datavalidator 
# 
# Validation summary: 
#  Number of passed validations: 1
#  Number of failed validations: 2
#  Number of validations with warnings: 1
# 
# Advanced view: 
# 
# 
# |object      |title                         |result  |validation_id        |
# |:-----------|:-----------------------------|:-------|:--------------------|
# |sample_data |x should have character class |Passed  |B4u28kcRoVKIFkJGe0VD |
# |sample_data |y should have numeric class   |Failed  |noIe1TSxa9e3pMmJekOW |
# |sample_data |y should have date class      |Failed  |HfXDBnL5nYIBPfBXpehb |
# |sample_data |z should have Date class      |Warning |p7pW4xo4jEpoKf1HRv15 |

# If we want to do some action depending on validation results we can use `get_validations()` method
if (validator$get_validations()$n_failed > 0) print("We've got a problem!")
# "We've got a problem!"

# To save report log we have to call `generate_report_log()` method
validator$save_log(output_path = "my_log_path")
```

Go to `example/LOG` folder to see the full example.

# To build package in docker use:

```
devtools::build(path = "/mnt/package")
```
