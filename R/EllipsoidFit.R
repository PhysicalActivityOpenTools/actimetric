#' Title
#'
#' @param ag
#'
#' @return
#' @export
#'
#' @examples
EllipsoidFit<-function(ag){

  sqrnorms<-rowSums(ag^2)


  Xextend<-matrix(1,4,nrow(ag))
  Xextend[2:4,]<-t(ag)

  A<-Xextend%*%t(Xextend)
  b<-(-Xextend)%*%sqrnorms

  result<-solve(A,b)

  offset<-result[2:4,1]/2

  x<-rep(offset[1],nrow(ag))
  y<-rep(offset[2],nrow(ag))
  z<-rep(offset[3],nrow(ag))
  olong<-cbind(x,y,z)

  X<-ag+olong
  Xsqr<-X^2
  A<-t(Xsqr)%*%Xsqr
  b<-t(Xsqr)%*%rep(1,nrow(ag))
  result<-sqrt(solve(A,b))
  scale<-1/result


  return(list(offset = offset,scale = scale))

}
