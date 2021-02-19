#' Find all chain parts in parent frame
find_chain_parts <- function() {
  i <- 1
  while(!("chain_parts" %in% ls(envir=parent.frame(i))) && i < sys.nframe()) {
    i <- i+1
  }
  parent.frame(i)
}

#' Get first name of the data frame
#' @param df data.frame
#' @return deparesed chain part
get_first_name <- function(df){
  ee <- find_chain_parts()
  deparse(ee$lhs)
}

#' Generate a random ID.
#'
#' @return A characters corresponding to random ID.
generate_id <- function () {
  paste0(paste0(sample(c(LETTERS, letters, 0:9), 5, TRUE),
                collapse = ""), round(as.numeric(Sys.time()) * 1000))
}

#' Detect type of predicate
#'
#' @param predicate Predicate or predicate generator function.
#' @return String informing about predicate type.
#'   When "direct" the predicate is directly applied to the column.
#'   When "generator" the predicate returns predicate computed based on a column.
#'   When NA, the predicate shouldn't be used at all in the validation.
get_predicate_type <- function(predicate) {
  predicate_output <- predicate(0:1)
  if (is.logical(predicate_output)) {
    return("direct")
  }
  if (is.function(predicate_output)) {
    return("generator")
  }
  return(NA_character_)
}
