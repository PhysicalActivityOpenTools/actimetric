#' Converts tick to posix
#'
#' @param x Tick
#' @param tz Time zone
#' @param ... Additional arguments for as.POSIXct.
#'
#' @return POSIXct object
#' @export
#' @author Matthew N. Ahmadi <matthew.ahmadi@sydney.edu.au>
tick_to_posix <- function(x, tz = "UTC", ...) {
  # Original code provided by Matthew N. Ahmadi
  x <- as.numeric(as.character(x)) / 10000000

  as.POSIXct(x, tz, origin = "0001-01-01", ...)

}
