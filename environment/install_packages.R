library(devtools)

install_github("Appsilon/verified.installation")

library(verified.installation)
install_and_verify(package = "testthat")
install_and_verify(install = install_github, package = "shiny.semantic", package_path = "Appsilon/shiny.semantic", ref = "477343a87f5d6eccc73a34b1cbe76f5adf40de8f")
install_and_verify(package = "rmarkdown")
