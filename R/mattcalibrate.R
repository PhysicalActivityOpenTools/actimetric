#' Title
#'
#' @param raw
#' @param Fs
#'
#' @return
#' @export
#'
#' @examples
calibrate<-function(raw,Fs){

  cat("Calibration in Progress\n")
  if(nrow(raw)>(72*60*60*Fs)){
  raw<-raw[1:(72*60*60*Fs),]
  }
  Gx<-slide(raw[,2],30*Fs,FUN=mean)
  Gy<-slide(raw[,3],30*Fs,FUN=mean)
  Gz<-slide(raw[,4],30*Fs,FUN=mean)
  Gx<-cbind(Gx,Gy,Gz)
  vm<-sqrt(rowSums(raw[,2:4]^2))
  vm.sd<-slide(vm,30*Fs,FUN=sd)
  vm<-slide(vm,30*Fs,FUN=mean)
  e<-which(vm.sd<.013 & vm<2 & vm>0)
  Gx<-Gx[e,]
  vm<-vm[e]
  hrs<-round(nrow(Gx)/2/60,2)
  cat(paste("Hours Used: ",hrs,"\n\n",sep = ""))
  cat("-----Before Calibration Results-----\n")

  vm.error.st<-round(mean(abs(vm-1))*1000,2)
  cat(paste("VM Error (mg's): ",vm.error.st,"\n",sep = ""))
  aa<-center_radius(Gx)
  cat(paste("Center Error (x,y,z): ","(", aa$center[1],",",aa$center[2],",",
            aa$center[3],")\n",sep = ""))
  cat(paste("Radius Error: ",aa$radius,"\n",sep = ""))

  cat("\n-----After Calibration Results-----\n")
  ag<-EllipsoidFit(Gx)
  Gx[,1]<-ag$scale[1]*Gx[,1]+ag$offset[1]
  Gx[,2]<-ag$scale[2]*Gx[,2]+ag$offset[2]
  Gx[,3]<-ag$scale[3]*Gx[,3]+ag$offset[3]

  vm<-sqrt(rowSums(Gx^2))
  vm.error.end<-round(mean(abs(vm-1))*1000,2)
  cat(paste("VM Error (mg's): ",vm.error.end,"\n",sep = ""))

  aa<-center_radius(Gx)
  cat(paste("Center Error (x,y,z): ","(", aa$center[1],",",aa$center[2],",",
            aa$center[3],")\n",sep = ""))
  cat(paste("Radius Error: ",aa$radius,"\n",sep = ""))
  return(list(offset = ag$offset,scale = ag$scale,vm.error.end = vm.error.end,
              vm.error.st = vm.error.st))

}
