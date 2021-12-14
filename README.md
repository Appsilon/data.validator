<a href = "https://appsilon.com/careers/" target="_blank"><img src="http://d2v95fjda94ghc.cloudfront.net/hiring.png" alt="We are hiring!"/></a>


[![R-CMD-check](https://github.com/Appsilon/data.validator/workflows/R-CMD-check/badge.svg)](https://github.com/Appsilon/data.validator/actions)
[![Codecov test coverage](https://codecov.io/gh/Appsilon/data.validator/branch/master/graph/badge.svg)](https://codecov.io/gh/Appsilon/data.validator?branch=master)
[![cranlogs](https://cranlogs.r-pkg.org/badges/data.validator)](https://CRAN.R-project.org/package=data.validator)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/data.validator)](https://CRAN.R-project.org/package=data.validator)

<img src="assets/hexsticker.png" width="160px" align="right"/>

data.validator
==============

## Description

`data.validator` is a package for scalable and reproducible data validation. It provides:

* Functions for validating datasets in `%>%` pipelines: `validate_if`, `validate_cols` and `validate_rows`
* Predicate functions from [assertr](https://github.com/ropensci/assertr) package, like `in_set`, `within_bounds`, etc.
* Functions for creating user-friendly reports that you can send to email, store in logs folder, 
  or generate automatically with RStudio Connect.

![](assets/semantic_report_example.gif)

## Installation

Install from CRAN:

```r
install.packages("data.validator")
```

or the latest development version:

```r
remotes::install_github("Appsilon/data.validator")
```

## Data validation

Validaton cycle is simple:

1. Create report object.
2. Prepare your dataset. You can load it, preprocess and then run `validate()` pipeline.
2. Validate your datasets.
    * Start validation block with `validate()` function. It adds new section to the report.
    * Use `validate_*` functions and predicates to validate the data. You can create your custom predicates. See `between()` example.
    * Add assertion results to the report with `add_results()`
3. Print the results or generate HTML report.

```r
library(assertr)
library(magrittr)
library(data.validator)

report <- data_validation_report()

validate(mtcars, name = "Verifying cars dataset") %>%
  validate_if(drat < 0, description = "Column drat has only positive values") %>%
  validate_cols(in_set(c(0, 2)), vs, am, description = "vs and am values equal 0 or 2 only") %>%
  validate_cols(within_n_sds(1), mpg, description = "mpg within 1 sds") %>%
  validate_rows(num_row_NAs, within_bounds(0, 2), vs, am, mpg, description = "not too many NAs in rows") %>%
  validate_rows(maha_dist, within_n_mads(10), everything(), description = "maha dist within 10 mads") %>%
  add_results(report)

between <- function(a, b) {
  function(x) { a <= x && x <= b }
}

validate(iris, name = "Verifying flower dataset") %>%
  validate_if(Sepal.Length > 0, description = "Sepal length is greater than 0") %>%
  validate_cols(between(0, 4), Sepal.Width, description = "Sepal width is between 0 and 4") %>%
  add_results(report)

print(report)
```


## Reporting

Print results to the console:

```r
print(report)

# Validation summary: 
#  Number of successful validations: 2
#  Number of failed validations: 3
#  Number of validations with warnings: 0
# 
# Advanced view: 
# 
# 
# |table_name             |description                          |type    | total_violations|
# |:----------------------|:------------------------------------|:-------|----------------:|
# |Verifying cars dataset |Column drat has only positive values |error   |               32|
# |Verifying cars dataset |maha dist within 10 mads             |success |               NA|
# |Verifying cars dataset |mpg within 1 sds                     |error   |                8|
# |Verifying cars dataset |not too many NAs in rows             |success |               NA|
# |Verifying cars dataset |vs and am values equal 0 or 2 only   |error   |               27|
```


Save as HTML report

```r
save_report(report)
```

## Full examples

- [Custom reporting on leaflet map](https://github.com/Appsilon/data.validator/blob/master/examples/custom_report/example.R)

![](assets/custom_report_example.gif)

- [Minimal example to get you started](https://github.com/Appsilon/data.validator/blob/master/examples/minimal_example/example.R)

- [Convenient API](https://github.com/Appsilon/data.validator/blob/master/examples/new_api/example.R)

- [Various way of saving reports](https://github.com/Appsilon/data.validator/blob/master/examples/sample_validations/example.R)


## Using custom report templates

In order to generate rmarkdown report `data.validator` uses predefined report template.
You may find it in `inst/rmarkdown/templates/standard/skeleton/skeleton.Rmd`.

The report contains basic requirements for each report template used by `save_report` function:

- defining params

```
params:
  generate_report_html: !expr function(...) {}
  extra_params: list()
```

- calling content renderer chunk

````
```{r generate_report, echo = FALSE}
params$generate_report_html(params$extra_params)
```
````

If you want to use the template as a base you can use RStudio.
Load the package and use `File -> New File -> R Markdown -> From template -> Simple structure for HTML report summary`.
Then modify the template adding custom title, or graphics with leaving the below points unchanged and specify the path inside `save_report`'s `template` parameter.

# How the package can be used in production?

The package was successfuly used by Appsilon in production enviroment for protecting Shiny Apps against beeing run on incorrect data.

The workflow was based on the below steps:

1. Running [RStudio Connect Scheduler](https://rstudio.com/products/connect/) daily.

2. Scheduler sources the data from PostgreSQL table and validates it based on predefined rules.

3. Based on validation results a new `data.validator` report is created.

4a. When data is violated:

- data provider and person responsible for data quality receives report via email

- thanks to `assertr` functionality, the report is easily understandable both for technical, and non-technical person

- data provider makes required data fixes  

4b. When data is correct:

- a specific trigger is sent in order to reload Shiny data 

The workflow is presented on below graphics
![](assets/workflow.png)

## Contributing

We welcome contributions of all types!

We encourage typo corrections, bug reports, bug fixes and feature requests. Feedback on the clarity of the documentation and examples is especially valuable.

If you want to contribute to this project please submit a regular PR, once you’re done with new feature or bug fix.

### Changes in documentation

Both repository **README.md** file and an official documentation page
are generated with markdown.

Documentation is rendered with `pkgdown`. Just run
`pkgdown::build_site()` after rendering new **README.md**.

## Appsilon

<img src="https://avatars0.githubusercontent.com/u/6096772" align="right" alt="" width="6%" />

Appsilon is the **Full Service Certified RStudio Partner**. Learn more
at [appsilon.com](https://appsilon.com).

Get in touch [support+opensource@appsilon.com](support+opensource@appsilon.com)
