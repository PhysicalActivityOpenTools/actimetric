#' Title
#'
#' @param x
#' @param domain
#' @param vector.mag
#' @param window
#' @param overlap
#' @param freq
#' @param lower
#' @param upper
#'
#' @return
#' @export
#'
#' @examples
feature.extraction<-function(x=trial2,domain=3,vector.mag= 3, window=10, overlap=0,freq=30,lower=.5,upper=5){

  window=window*freq
  if (overlap == 0)
  {
    width=window
  }else if (overlap != 0 & overlap < 1)
  {
    width=window/(1/overlap)
  }else if (overlap >= 1)
  {
    stop("

         ERROR: overlap value must be less than 1

         ")
  }
  if(!isTRUE(upper%in%NA) & upper>(freq/2))
  {
    stop("
         ERROR: upper bound must be =<50% of sampling frequency

         ")
  }

  colnames(x)= c("x","y","z")
  vm<-sqrt((x[,1]^2)+(x[,2]^2)+(x[,3]^2))
  Enmo<- vm-1
  Enmo[Enmo<0]<- 0
  tilt<- acos(x[,2]/vm)*(180/pi)


  if (domain== 3 & vector.mag== 2)
  {
    td<-slide(vm,window,FUN=time.domain,by=width)
    fd<-slide(vm,window,FUN=function(y) freq.domain(y,SampFreq=freq,lower,upper),by=width)
    xy<-cross.corr(x[,1],x[,2], window,by=width)
    xz<-cross.corr(x[,1],x[,3], window,by=width)
    yz<-cross.corr(x[,2],x[,3], window,by=width)
    Enmo<- slide(Enmo,window,FUN=mean,by=width)
    tilt<- slide(tilt,window,mean,by=width)

    all.vector.magnitude= data.frame(vm=td,vm=fd,xy,xz,yz,Enmo,tilt)
    return(all.vector.magnitude)
  }
  }
