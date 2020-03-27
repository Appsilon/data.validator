<link href="http://fonts.googleapis.com/css?family=Maven+Pro:400,700|Inconsolata" rel="stylesheet" type="text/css"> <link href='docs/style.css' rel='stylesheet' type='text/css'>

[![Codecov test coverage](https://codecov.io/gh/Appsilon/data.validator/branch/master/graph/badge.svg)](https://codecov.io/gh/Appsilon/shiny.info?branch=master)

data.validator
==============

# Description

`data.validator` is a set of tools for creating reports based on [assertr's](https://github.com/ropensci/assertr) validation results.

# How to validate data with assertr

## Basic example

```
library(assertr)
library(dplyr)
mtcars %>%
  chain_start(store_success = TRUE) %>%
  assert(description = "vs and am values equal 0 or 2 only", in_set(c(0, 2)), vs, am) %>%
  assert(description = "vs and am values should equal 3 or 4", skip_chain_opts = TRUE,
         error_fun = warning_append, in_set(c(3, 4)), gear, carb) %>%
  assert_rows(description = "Each row sum for am:vs columns is less or equal 1", rowSums, within_bounds(0, 1), vs:am) %>%
  insist(description = "For wt and qsec we have: abs(col) < 2 * sd(col)", within_n_sds(2), wt, qsec) %>%
  verify(description = "Column drat has only positive values", drat > 0) %>%
  verify(description = "Column drat has only values larger than 3", drat > 3) %>%
  chain_end(error_fun = error_append)
```

For full specification see [assertr vignette](https://docs.ropensci.org/assertr/).

# How to use data.validator for presenting the results

1. Create new validator
```
library(data.validator)
validator <- create_validator()
```

2. Add results to created object
```
library(assertr)
library(dplyr)
mtcars %>%
  chain_start(store_success = TRUE) %>%
  assert(description = "vs and am values equal 0 or 2 only", in_set(c(0, 2)), vs, am) %>%
  assert(description = "vs and am values should equal 3 or 4", skip_chain_opts = TRUE,
         error_fun = warning_append, in_set(c(3, 4)), gear, carb) %>%
  assert_rows(description = "Each row sum for am:vs columns is less or equal 1", rowSums, within_bounds(0, 1), vs:am) %>%
  insist(description = "For wt and qsec we have: abs(col) < 2 * sd(col)", within_n_sds(2), wt, qsec) %>%
  verify(description = "Column drat has only positive values", drat > 0) %>%
  verify(description = "Column drat has only values larger than 3", drat > 3) %>%
  chain_end(error_fun = error_append) %>%
  add_results(validator)
```

3. Use one of available methods to present results

- print summary
```
print(validator)

# Validation summary: 
#  Number of successful validations: 1
#  Number of failed validations: 4
#  Number of validations with warnings: 1
# Advanced view: 
# 
# 
# |table_name |description                                       |type    | total_violations|
# |:----------|:-------------------------------------------------|:-------|----------------:|
# |mtcars     |Column drat has only positive values              |success |               NA|
# |mtcars     |Column drat has only values larger than 3         |error   |                4|
# |mtcars     |Each row sum for am:vs columns is less or equal 1 |error   |                7|
# |mtcars     |For wt and qsec we have: abs(col) < 2 * sd(col)   |error   |                4|
# |mtcars     |vs and am values equal 0 or 2 only                |error   |               27|
# |mtcars     |vs and am values should equal 3 or 4              |warning |               24|
```
- save as HTML report

```
save_report(validator)
```

![](assets/semantic_report_example.gif)

## Creating custom reports

Define function of three parameters `n_passed`, `n_failed`, `n_warned`, `validation_results` and returns HTML object or HTML widget.

*Note*
data.validator automatically filters out `validation_results` passed to the function based on `save_report`'s `summary parameter`

In this example we create custom report that shows validation results of checking wheter population across polish counties fits within 3 standard deviations. The results are shown on leaflet map.

```
library(datavalidator)
library(magrittr)
library(assertr)

validator <- create_validator()

population <- read.csv("population.csv", colClasses = c("character", "character", "character", "integer", "integer", "integer"))
population %>%
  insist(within_n_sds(3), total, success_fun = success_append, error_fun = error_append) %>%
  add_results(validator)

validator

# Validation summary: 
#  Number of successful validations: 0
#  Number of failed validations: 1
#  Number of validations with warnings: 0
# Advanced view: 
# 
# 
# |table_name |description |type  | total_violations|
# |:----------|:-----------|:-----|----------------:|
# |population |NA          |error |                6|

render_leaflet_report <- function(n_passed, n_failed, n_warned, validation_results) {
  states <- rgdal::readOGR("counties.shp", GDAL1_integer64_policy = TRUE, verbose = FALSE)
  population <- read.csv("population.csv", colClasses = c("character", "character", "factor", "integer", "integer", "integer"))
  violated <- validation_results %>%
    tidyr::unnest(error_df, keep_empty = TRUE) %>%
    dplyr::pull(index)
  states@data <- dplyr::left_join(states@data, population, by = c("JPT_KOD_JE" = "county_ID"))
  states@data$color <- "#52cf0a"
  states@data$color[violated] <- "#bf0b4d"
  htmltools::tagList(
    htmltools::h2("Counties not fitting within 3 standard deviations"),
    leaflet::leaflet(states) %>%
      leaflet::addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.5, fillOpacity = 0.5,
                  fillColor = states@data$color,
                  label = glue::glue("County: {states@data$county} <br> Population: {states@data$total}") %>% lapply(htmltools::HTML),
                  highlightOptions = leaflet::highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
  )
}

save_report(validator, ui_constructor = render_leaflet_report)
```

The resulting report
![](assets/custom_report/custom_report_example.gif)

# Using custom report templates

In order to generate rmarkdown report data.validator uses predefined report template.
You may find it [here](inst/rmarkdown/templates/standard/skeleton/skeleton.Rmd).

The report contains basic requirements for each report template used by `save_report` function:
- defining params
```
params:
  generate_report_html: !expr function(...) {}
  summary: !expr c("success", "warning", "error")
  report_ui_constructor: !expr render_raw_report_ui
```
- calling content renderer chunk
````
```{r generate_report, echo = FALSE}
params$generate_report_html(params$summary, params$report_ui_constructor)
```
````

If you want to use the template as a base you can use RStudio.
Load the package and use `File -> New File -> R Markdown -> From template -> Simple structure for HTML report summary`.
Then modify the template adding custom title, or graphics with leaving the below points unchanged and specify the path inside `save_report`'s `template` parameter.

# How can the package be used in production

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

# More examples

For more options check package documentation or [examples](inst/examples).
