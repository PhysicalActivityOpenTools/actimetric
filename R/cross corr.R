#' Title
#'
#' @param a
#' @param b
#' @param width
#' @param by
#'
#' @return
#' @export
#'
#' @examples
cross.corr<-function(a,b,width,by){
  lenX <- length(a)
  QUT1 <- seq(1, lenX - width + 1, by = by)
  QUT1.1 <- seq(width, lenX , by = by)
  a <- sapply(1:length(QUT1), function(i) a[QUT1[i]:QUT1.1[i]])
  colnames(a)<-c(1:dim(a)[2])

  lenX <- length(b)
  QUT1 <- seq(1, lenX - width + 1, by = by)
  QUT1.1 <- seq(width, lenX , by = by)
  b <- sapply(1:length(QUT1), function(i) b[QUT1[i]:QUT1.1[i]])
  colnames(b)<-c(1:dim(b)[2])

  cols <- intersect(colnames(a), colnames(b))

  # For each column, compute cor
  res <- lapply(cols, function(x) cor(
    a[, x],
    b[, x]
  ))
  res[is.na(res)]<-0
  return(do.call(rbind,res))
}
