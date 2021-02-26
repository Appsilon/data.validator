library(magrittr)
library(data.validator)
library(rgdal)
library(dplyr)
library(tidyr)
library(glue)
library(leaflet)
library(htmltools)

#' Define function of `validation_results` parameter that returns HTML object or HTML widget.
#' The `validation_results` parameter is assumed to be passed as a results table
#' extracted with `get_results(validator)`.

#' *Note* The function can also store optional parameters that should be passed to
#' `save_report` function while generating a new report.

#' In this example we create custom report that shows validation results of
#' checking wheter population across Polish counties fits within 3 standard deviations.
#' The results are shown on leaflet map.

validator <- data_validation_report()

file <- system.file("extdata", "population.csv", package = "data.validator")
population <- read.csv(file, colClasses = c("character", "character", "character", "integer", "integer", "integer"))
validate(population) %>%
  validate_cols(assertr::within_n_sds(3), total) %>%
  add_results(validator)
validator

render_leaflet_report <- function(validation_results, population_data, correct_col, violated_col) {
  file <- system.file("extdata", "counties.json", package = "data.validator")
  states <- rgdal::readOGR(file, GDAL1_integer64_policy = TRUE, verbose = FALSE)

  violated <- validation_results %>%
    tidyr::unnest(error_df, keep_empty = TRUE) %>%
    dplyr::pull(index)

  states@data <- dplyr::left_join(states@data, population_data,
                                  by = c("JPT_KOD_JE" = "county_ID"))
  states@data$color <- correct_col
  states@data$color[violated] <- violated_col
  states@data$label <- glue::glue("County: {states@data$county} <br>",
                                  "Population: {states@data$total}")

  htmltools::tagList(
    htmltools::h2("Counties not fitting within 3 standard deviations"),
    leaflet::leaflet(states) %>%
      leaflet::addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                           opacity = 0.5, fillOpacity = 0.5,
                           fillColor = states@data$color,
                           label = states@data$label %>% lapply(htmltools::HTML),
                           highlightOptions = leaflet::highlightOptions(color = "white",
                                                                        weight = 2,
                                                                        bringToFront = TRUE))
  )
}

save_report(
  validator,
  ui_constructor = render_leaflet_report,
  population_data = population,
  correct_col = "#52cf0a",
  violated_col = "#bf0b4d"
)
browseURL("validation_report.html")
