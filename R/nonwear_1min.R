#' Title
#'
#' @param data
#' @param Fs
#' @param window
#'
#' @return
#' @export
#'
#' @examples
nonwear_vm<-function(data,Fs,window){

  Gx = as.numeric(data[,1]); Gy = as.numeric(data[,2]); Gz = as.numeric(data[,3])
  vm<-sqrt(Gx^2+Gy^2+Gz^2)

  vm<-slide(vm,width = window*Fs,FUN=sd)

  vm2<-rep(0,length(vm))
  e<-which(vm<0.013) #SD threshold
  vm2[e]<-1
  run<-rle(vm2)
  run<-rep(run$lengths,run$lengths)
  vm2<-cbind(vm2,run)

  e<-which(vm2[,1]==1 & vm2[,2]>=60/window*30) #index all values where SD is below threshold for at least 30 minutes, it is nonwear
  nonwear<-rep(0,length(vm))
  nonwear[e]<-1

  check<-nonwear
  run<-rle(check)
  run<-cbind(run[[1]],run[[2]])


  i=1
  ii=1
  wear<-matrix(0,length(which(run[,2]==0)),3) #create a table with row length equal to wear periods
  while(i<=nrow(run)){
    if(run[i,2]==0){
      if(i>1){wear[ii,1]<-run[i-1,1]} #non-wear before wear period
      wear[ii,2]<-run[i,1] #wear period
      if(i<nrow(run)){wear[ii,3]<-run[i+1,1]} #non-wear after wear period

      if (wear[ii, 2] < ((60/window*30)) & (wear[ii,2]/(wear[ii, 1] + wear[ii, 3])) < 0.3) {
        run[i,2] = 1
        #' if wear period is less than 30 min and less than 30% of bordering non-wear period,
        #' convert to nonwear
      }
      ii<-ii+1
    }
    i<-i+1
  }
  check<-rep(run[,2],run[,1])


  nonwear = nonwear+check #sum original nonwear detection plus the wear periods that were checked for non-wear
  nonwear[nonwear>=1]<-1 #any nonwear >=1 is nonwear and make all values 1 to make binary outcome; 0/1


  return(nonwear)
}
