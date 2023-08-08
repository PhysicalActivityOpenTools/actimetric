#' Title
#'
#' @param x
#' @param lag
#'
#' @return
#' @export
#'
#' @examples
lag<-function(x,lag)
{
  lag1=acf(x, plot = FALSE, lag.max = lag)$acf[1+lag]
  if (is.na(lag1))
    lag1 = 0
  return(lag1)
}
