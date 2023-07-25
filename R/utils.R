#' Recursively constructs Abstract Syntax Tree for a given expression
#' @keywords internal
getAST <- function(ee) purrr::map_if(as.list(ee), is.call, getAST)

#' @keywords internal
is_complex_command <- function(command_string) {
  # If the string contains any characters that are not alphanumeric or underscore,
  # it is considered a complex command
  return(grepl("[^a-zA-Z0-9_]", command_string))
}

# Create a recursive function to find the first non-call object
#' @keywords internal
find_first_noncall <- function(object) {
  # browser()
  if (is.list(object) | any(is_complex_command(deparse(object)))) {
    return(find_first_noncall(object[[2]]))
  } else {
    return(object)
  }
}

#' Get name of the data frame based on active frames
#' @return deparsed chain part
#' @keywords internal
get_first_name <- function() {
  # browser()
  sc <- sys.calls()
  first_call_with_pipe <- purrr::map(as.list(sc), getAST) %>%
    purrr::keep(~identical(.[[1]], quote(`%>%`)))
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
