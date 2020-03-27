# Description

`datavalidator` is a set of tools for creating reports based on [assertr's]() validation results.

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

For full specification see [assertr vignette]().

# How to use data.validator for presenting the results

1. Create new validator
```
library(data.validator)
validator <- create_validator()
```

2. Add results to created object
```
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

![](examples/semantic_report_example.gif)

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
![](examples/custom_report/custom_report.gif)


For more options check package documentation or [examples](examples).
