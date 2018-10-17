# Description

`datavalidator` is a set of tools for creating a HTML validation report.

# How to use it

1. Create a new validation report template.

To create new template open `File -> New file -> R Markdown...` and select `Appsilon Validation Report Template (datavalidator)` form `From template` tab.

2. The newly created report template has predefined structure. Add `R` scripts to run in the validation process:
```
- VALIDATION_REPORT_FOLDER
|- VALIDATION_REPORT.Rmd
|- logo.png
|- {R_SCRIPTS.R}
```

The `VALIDATION_REPORT.Rmd` file is responsible for HTML report generation and it depends on remaining 4 files.
The `logo.png` file contains logo That will be shown at the top of the generated report.
The `{R_SCRIPTS.R}` are additional files to run inside the report (data reading, validations, e.t.c)

EXAMPLE:

```{r}
library(datavalidator)
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
library(assertr)
library(dplyr)

xyz <- tibble(
x = letters[1:3],
y = letters[1:3],
z = 1:3)

xyz <- xyz %>%
clear_results() %>%  chain_start() %>%
verify(title = "x should have character class", v_class(x) == "character") %>%
verify(title = "y should have numeric class", v_class(y) == "numeric") %>%
verify(title = "y should have date class", v_class(y) == "Date") %>%
verify(title = "z should have Date class", ignore_chain_funs = TRUE,
error_fun = append_as_warning, v_class(z) == "Date") %>%
chain_end(error_fun = error_append)

validator$add_validations(xyz)

validator
# Github repo path:  https://github.com/my_account/my_proj/ 
# 
# Validation summary: 
#  Number of fulfilled validations: 1
#  Number of failed validations: 2
#  Number of validations with warnings: 1
# 
# Advanced view: 
# # A tibble: 4 x 4
#   object title                         result  validation_id       
#   <chr>  <chr>                         <chr>   <chr>               
# 1 xyz    x should have character class Passed  GXOz0f4EECjSKqEhYosG
# 2 xyz    y should have numeric class   Failed  6aiSSKNCwnluPtFqqZEe
# 3 xyz    y should have date class      Failed  27D28qprln3aYIMGzHAi
# 4 xyz    z should have Date class      Warning K0vR9gCPGrNGt1wO70ZU

# To generate a html report we have to call `generate_html_report()` method
library(shiny)
validator$generate_html_report()
```

Go to `example` folder to see the full example.

3. Run `datavalidator::generate_report()` function to generate the report, for example:

```
datavalidator::generate_report(
 template = "example/example.Rmd", 
 output_dir = "example/",
 output_file = "validation_report.html",
 repo_path = "https://github.com/Appsilon/datavalidator/tree/refactor_code",
 scripts = c("prepare_data.R", "validation_rules.R"))
```

# To build package in docker use:

```
devtools::build(path = "/mnt/package")
```
