#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
time.domain<-function(x)
{
  mean<-mean(x)
  sd<-sd(x)
  cv<-sd/mean
  perc<-rbind(quantile(x,p=c(.10,.25,.50,.75,.90)))
  skew<-skewness(x)
  if(is.na(skew))
    skew=0
  kurt<-kurtosis(x)
  if(is.na(kurt))
    kurt=0
  max<-max(x)
  min<-min(x)
  p2p<-max-min
  q<-as.numeric(quantile(x,p=.5))####median crossing calculation start
  b<-ifelse(x>q,1,0)
  d<-diff(b)
  e<-which(d>0)
  e<-as.numeric(length(e))

  b<-ifelse(x<q,1,0)
  d<-diff(b)
  f<-which(d>0)
  f<-as.numeric(length(f))
  median<-f+e #####median crossing calcuation end
  sum<-sum(x)
  mad<-mad(x)
  power<-sum(x^2)
  ACF=lag(x,lag=1)
  logen<- sum(log(x^2))
  g<-quantile(x,p=.25)
  h<-quantile(x,p=.75)
  iqr<-h-g

  metric<- cbind(mean,sd,cv, p10=perc[,1],p25=perc[,2],p50=perc[,3],p75=perc[,4],p90=perc[,5],
                 skew,kurt,max,min,p2p,median,sum,mad,power,ACF,logen,iqr)
  return(metric)
}
