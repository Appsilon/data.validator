#' Constructs an Abstract Syntax Tree for an expression
#'
#' This function breaks down an R expression into a list structure,
#' creating a tree-like representation of the code.
#'
#' @param exp An R expression to be parsed into a list structure.
#' @return A list structure that represents the input R expression.
#' @keywords internal
get_ast <- function(exp) purrr::map_if(as.list(exp), is.call, get_ast)

#' Check if a command is complex, i.e, contains any non-alphanumeric character
#'
#' @param command_string A character string representing the command to be checked.
#' @return Logical value indicating whether the command_string is complex (TRUE) or not (FALSE).
#' @keywords internal
is_complex_command <- function(command_string) {
  return(grepl("[^a-zA-Z0-9_]", command_string))
}

#' Create a recursive function to find the first non-call object
#'
#' This function iteratively dives into the provided list (R expression),
#' until it finds an object that is not a function call or a complex command.
#' The [[2]] is used with the argument 2, because in the list representation
#' of function calls in R, the actual function is the first element, and its arguments
#' are the subsequent elements. So object 2 generally refers to the first argument
#' of the function call.
#' @param object A list representing an R expression.
#' @return The first non-call object found in the list representation of an R expression.
#' @keywords internal
find_first_noncall <- function(object) {
  if (is.list(object) | any(is_complex_command(deparse(object)))) {
    return(find_first_noncall(object[[2]]))
  } else {
    return(object)
  }
}


#' Extract the name of the initial data object in a magrittr pipe chain
#'
#' This function analyzes the call stack, identifies the first call using
#' the magrittr pipe operator (`%>%`), and then retrieves the name of the
#' initial object in that pipe chain.
#'
#' @return A string representing the name of the initial data object in the pipe chain.
#' @keywords internal
get_first_name <- function() {
  sc <- sys.calls()
  first_call_with_pipe <- purrr::map(as.list(sc), get_ast) %>%
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
