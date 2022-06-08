library(shiny)
library(data.validator)
library(magrittr)
library(assertr)

ui <- fluidPage(
  uiOutput("validation")
)

server <- function(input, output, session) {
  report <- data_validation_report()
  validate(mtcars, name = "Verifying cars dataset") %>%
    validate_if(drat > 0, description = "Column drat has only positive values") %>%
    validate_cols(in_set(c(0, 2)), vs, am, description = "vs and am values equal 0 or 2 only") %>%
    validate_cols(within_n_sds(1), mpg, description = "mpg within 1 sds") %>%
    validate_rows(num_row_NAs, within_bounds(0, 2), vs, am, mpg, description = "not too many NAs in rows") %>%
    validate_rows(maha_dist, within_n_mads(10), everything(), description = "maha dist within 10 mads") %>%
    add_results(report)

  output$validation <- renderUI({
    render_semantic_report_ui(get_results(report))
  })
}

shinyApp(ui, server)
