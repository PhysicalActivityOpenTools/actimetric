#' Title
#'
#' @param x
#' @param digits
#'
#' @return
#' @export
#'
#' @examples
round_df <- function(x, digits) {

  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


