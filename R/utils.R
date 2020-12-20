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
#' @return A characters corresponding to random ID.
generate_id <- function () {
  paste0(paste0(sample(c(LETTERS, letters, 0:9), 5, TRUE),
                collapse = ""), round(as.numeric(Sys.time()) * 1000))
}

#' Return description with datetime prefix.
#' @param description character
#' @return character
format_description <- function(description) {
  paste0("[", Sys.time(), "] ", description)
}
