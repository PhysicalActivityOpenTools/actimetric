#' Title
#'
#' @param count
#' @param axis
#' @param Sed
#' @param Mod
#' @param Vig
#'
#' @return
#' @export
#'
#' @examples
IntensityCounts<-function(count,axis="yaxis",Sed=200,Mod=420,Vig=842){


  cat(paste("\nApplying Cut-Points:\n",axis,"\n","Sed=",Sed,"\n","Mod=",Mod,"\n","Vig=",Vig,"\n",sep = " "))

  if(axis%in%"yaxis"){  counts<-count$Axis2}
  if(axis%in%"vm"){ counts<-sqrt(rowSums(count^2))}
  if(axis%in%"enmo"){counts<-count$enmo*1000}
  intensity<-rep(0,length(counts))
  e<-which(counts<=Sed)
  intensity[e]<-1
  e<-which(counts>Sed & counts<Mod)
  intensity[e]<-2
  e<-which(counts>=Mod & counts<Vig)
  intensity[e]<-3
  e<-which(counts>=Vig)
  intensity[e]<-4
  count<-cbind(count,intensity)
  cat("\nCompleted Intensity Classification\n")
  invisible(list(count=count,axis=axis,Sed=Sed,Mod=Mod,Vig=Vig))



}
