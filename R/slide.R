#' Compute Summary Statistics of Data Subsets
#'
#' @description
#' Splits the data into subsets, computes summary statistics for each, and
#' returns the result in a convenient form.
#'
#'
#' @param x Numeric vector to aggregate.
#' @param width Length of the window over which the aggregation will be computed.
#' @param by List of grouping elements, each as long as the variables in the
#' data frame x, or a formula. The elements are coerced to factors before use.
#' @param FUN Function to compute the summary statistics which can be applied to
#' all data subsets.
#' @param ... further arguments passed to or used by methods.
#'
#' @return Numeric vector withe the computed summary statistics.
#' @export
slide = function(x, width, by = NULL, FUN = NULL, ...) {
  FUN = match.fun(FUN)
  if (is.null(by)) by = width

  lenX = length(x)
  QUT1 = seq(1, lenX - width + 1, by = by)
  QUT2 = lapply(QUT1, function(x) x:(x + width - 1))

  QUT3 = lapply(QUT2, function(a) FUN(x[a], ...))
  QUT4 = do.call(rbind,QUT3)
  return(QUT4)
}
