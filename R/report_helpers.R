#' Generate a random ID.
#' @description Generate a random ID.
#' @return A characters corresponding to random ID.
#' @export
generate_id <- function() {
  paste0(sample(c(LETTERS, letters, 0:9), 20, TRUE), collapse = "")
}
