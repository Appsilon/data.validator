.run_project <- function(times, delay = 1) {
  if (times <= 0 | !interactive()) return()
  if (rstudioapi::isAvailable() && interactive()) {
    options(shiny.launch.browser = .rs.invokeShinyWindowExternal)
    if (!is.null(rstudioapi::getActiveProject())) {
      return()
    } else {
      setwd("/mnt")
      rstudioapi::openProject("/mnt/datavalidator.Rproj")
    }
  } else {
    later::later(~ .run_project(times - 1), delay)
  }
}

.First <- function() {
  cat("#######################\n")
  cat("Hey Appsilon Developer!\n")
  cat("#######################\n")
  .run_project(6)
}
