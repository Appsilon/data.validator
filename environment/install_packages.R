library(devtools)

install_github("Appsilon/verified.installation")

library(verified.installation)
install_and_verify(package = "testthat")
