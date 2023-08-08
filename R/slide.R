#' Title
#'
#' @param x
#' @param width
#' @param by
#' @param FUN
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
slide<-function(x, width, by = NULL, FUN = NULL, ...)
{
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width

  lenX <- length(x)
  QUT1 <- seq(1, lenX - width + 1, by = by)
  QUT2 <- lapply(QUT1, function(x) x:(x + width - 1))

  QUT3 <- lapply(QUT2, function(a) FUN(x[a], ...))
  QUT4 <- do.call(rbind,QUT3)
  return(QUT4)
}
