#' Title
#'
#' @param A
#' @param SampFreq
#' @param lower
#' @param upper
#'
#' @return
#' @export
#'
#' @examples
freq.domain <-function(A,SampFreq,lower=NA,upper=NA)
{
  N = as.numeric(length(A))
  I=abs(fft(A - mean(A))/sqrt(N))^2
  P=((4/N)*I)^0.5
  f=(0:(N/2))/N
  c=cbind(f*SampFreq,P[1:(N/2+ 1)])

  if (upper%in%NA | lower%in%NA)
  {
    FMmax<- max(c[,2])
    Fmax<- (domfreq=as.numeric(c[which.max(c[,2]),1]))
    Entropy = -sum(P * log2(P))#####SHANNON ENTROPY#######

    freq.feat<-cbind(Fmax,FMmax,Entropy)
    return(freq.feat)
  }
  else{

    c<-c[c[,1] >= lower & c[,1] <= upper,]
    FMmax<- max(c[,2])
    Fmax<- (domfreq=as.numeric(c[which.max(c[,2]),1]))
    if(length(Fmax)==0)
      Fmax=0
    Entropy = -sum(P * log2(P))#####SHANNON ENTROPY#######
    freq.feat<-cbind(Fmax,FMmax,Entropy)

    colnames(freq.feat)<-c(paste("Fmax_",lower,"-",upper,"hz",sep = ""),
                           paste("FMmax_",lower,"-",upper,"hz",sep = ""),
                           paste("Entropy_",lower,"-",upper,"hz",sep = ""))
    return(freq.feat)
  }}
