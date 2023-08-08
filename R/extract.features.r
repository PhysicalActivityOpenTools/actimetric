#' Title
#'
#' @param Axis
#' @param wind
#' @param SampFreq
#'
#' @return
#' @export
#'
#' @examples
extract.features <- function(Axis, wind, SampFreq) {
  z <- wind  #window size
  x <- 1
  l <- floor(length(Axis)/z)
  k = matrix(0,nrow=l,ncol=16)
  for (i in 1:l){
    A=Axis[x:z]
    N=length(A)
    I=abs(fft(A - mean(A))/sqrt(N))^2
    P=(4/N)*I
    f=(0:(N/2))/N
    c=cbind(f,P[1:(N/2+ 1)])
    c2=c[1:(wind/10),]
    c3=c[(wind/3+1):(wind/2+1),]
    k1 <- max(c2[,2])
    k2 <- max(c3[,2])
    k3<- SampFreq*(domfreq=as.numeric(c2[which.max(c2[,2]),1]))
    k4<- SampFreq*(domfreq=as.numeric(c3[which.max(c3[,2]),1]))
    k5 <- mean(A,trim=.05,na.rm=TRUE)
    k6 <- sd(A,na.rm=TRUE)
    k7 <- sum((A*A),na.rm=TRUE)
    k8 <- cor(Axis[x:(z-1)],Axis[(x+1):z],use="pairwise.complete.obs")
    k9 <- median(A,na.rm=TRUE)
    k10 <- mad(abs(A),na.rm=TRUE)
    k11 <- as.vector(quantile(A,c(.10),na.rm = TRUE))
    k12 <- as.vector(quantile(A,c(.25),na.rm = TRUE))
    k13 <- as.vector(quantile(A,c(.75),na.rm = TRUE))
    k14 <- as.vector(quantile(A,c(.90),na.rm = TRUE))
    k15 <-shannon.entropy(P)[1]
    k16<-sum(c[(wind/3+1):(wind/2+1),2])

    k[i,] = c(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15,k16)
    x <- x + wind
    z <- z + wind
  }
  return(k)
}

