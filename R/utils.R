#' Recursively constructs Abstract Syntax Tree for a given expression
#' @keywords internal
getAST <- function(ee) purrr::map_if(as.list(ee), is.call, getAST)

#' Get name of the data frame based on active frames
#' @return deparsed chain part
#' @keywords internal
get_first_name <- function() {
  # browser()
  sc <- sys.calls()
  first_call_with_pipe <- purrr::map(as.list(sc), getAST) %>%
    purrr::keep(~identical(.[[1]], quote(`%>%`)))

  # first_object <- dplyr::last(first_call_with_pipe)[[2]]

  # Create a recursive function to find the first non-call object
  find_first_noncall <- function(object) {
    if (is.list(object)) {
      return(find_first_noncall(object[[2]]))
    } else {
      return(object)
    }
  }

  first_object <- find_first_noncall(dplyr::last(first_call_with_pipe)[[2]])
  deparse(first_object)
}


#' Generate a random ID.
#'
#' @return A characters corresponding to random ID.
#' @keywords internal
generate_id <- function() {
  paste0(paste0(sample(c(LETTERS, letters, 0:9), 5, TRUE),
                collapse = ""), round(as.numeric(Sys.time()) * 1000))
}
