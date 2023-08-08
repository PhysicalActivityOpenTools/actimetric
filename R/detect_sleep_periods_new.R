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

      postch = abs(diff(tilt))
      postch = ifelse(postch<=5,1,0)
      postch<-c(postch,postch[length(postch)]) #repeat last instance to make the length of "postch" the same as "tilt"
      run = rle(postch)

      run<-rep(run$lengths,run$lengths)
      postch<-cbind(postch,run)

      e<-which(postch[,1]==1 & postch[,2]>=(5*(60/window)))

      sdl1[e]<-1

      sleepperiod[start[i]:end[i]]<-sdl1
    }

  }
  return(sleepperiod)
}
