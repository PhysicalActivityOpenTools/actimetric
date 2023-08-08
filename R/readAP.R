#' Title
#'
#' @param file.and.path
#'
#' @return
#' @export
#'
#' @examples
read.activpal<- function(file.and.path)
{
  data<- fread(file.and.path,data.table = F)
  data<-data[,c(-2)]
  data$Time<- as.POSIXlt(data$Time*(60*60*24)+as.POSIXlt("1899-12-30"))
  data$X<-sapply(data$X,FUN = function(x)(x-128)/64)
  data$Y<-sapply(data$Y,FUN = function(x)(x-128)/64)
  data$Z<-sapply(data$Z,FUN = function(x)(x-128)/64)
  return(data)
}
