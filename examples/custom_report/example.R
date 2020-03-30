library(data.validator)
library(magrittr)
library(assertr)

validator <- create_validator()

population <- read.csv("population.csv", colClasses = c("character", "character", "character", "integer", "integer", "integer"))
population %>%
  insist(within_n_sds(3), total, success_fun = success_append, error_fun = error_append) %>%
  add_results(validator)
validator

render_leaflet_report <- function(n_passed, n_failed, n_warned, validation_results) {
  if (is.null(n_failed)) {
    return(htmltools::tags$div("Please add summary = c('error') to display vilated rules."))
  }
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
