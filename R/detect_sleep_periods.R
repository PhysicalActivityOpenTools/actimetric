#' Title
#'
#' @param data
#' @param window
#'
#' @return
#' @export
#'
#' @examples
detect_sleep_periods<-function(data, window){

  sleepperiod<-rep(0,nrow(data))

  if(any(data$class==6)==T){ #determine if at least 1 sleep window was detected for participant

    sleep<-ifelse(data$class==6,1,0) #make all instances into binary sleep 1/0
    sleep<-c(0,diff(sleep))
    start<-which(sleep==1) #identify start of each sleep window
    end<-which(sleep==-1) #identify end of each sleep window
    if(!length(end)==length(start)) {end<-c(end,nrow(data))} #in case monitoring period ends during a sleep window

    for (i in 1:length(start)){ #loop through all sleep windows

      tilt<-data$tilt[start[i]:end[i]]
      sdl1<-rep(0,length(tilt))

      postch = which(abs(diff(tilt)) > 5) #identify when tilt changes by more than 5 degrees between instances
      if (length(postch) > 1) {
        s1 = which(diff(postch) > (5*(60/window))) #identify when there is more than 5 minutes between tilt changes > 5 degrees
      }else{s1 = c()}
      if (length(s1) > 0) {
        try(for (gi in 1:length(s1)) {
          sdl1[postch[s1[gi]]:postch[s1[gi]+1]] = 1 #periods with less than 5 degree change between instances for at least 5 minutes = 1
        },silent=T)
      }else{
        sdl1[1:length(sdl1)]<-0 #no periods during sleep window had less than 5 degree change between instances for at least 5 minutes
      }

      sleepperiod[start[i]:end[i]]<-sdl1
    }

  }
  return(sleepperiod)
}
