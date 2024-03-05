#' Detects Sleep Periods from Thigh Attachment Site
#'
#' @param ts Data frame with ts object from \link{extractFeatures}
#' @param epoch Numeric with the epoch length in seconds.
#' @param sleep_id Identification number for sleep periods.
#' @param nonwear_id Identification number for nonwear periods.
#'
#' @return
#' @export
detectSleepThigh = function(ts, epoch, sleep_id, nonwear_id) {
  # Mode <- function(x) {
  #   ux <- unique(x)
  #   ux[which.max(tabulate(match(x, ux)))]
  # }
  # prepare ts
  ts$sleep = NA
  time = strptime(ts$time, "%H:%M:%S")
  time = format(time, "%H:%M:%S")
  ts$time = time
  # get metrics
  x = slide(ts$bfsd.x, width = (60/epoch)*5, FUN = DescTools::Median, by = 1)
  x2 = slide(ts$inc.x, width = (60/epoch)*5, FUN = DescTools::Median, by = 1)
  x2 = which(x < 0.1 & x2 < 135)
  x = rep(NA, length(x))
  x[x2] = 1
  xangle = slide(ts$fb.y, width = (60/epoch)*5, FUN = DescTools::Median, by = 1)
  # no movement
  nomov = rep(0, length(x))
  inbedtime = rep(NA, length(x))
  legangle =  rep(0, length(x))
  nomov[which(x == 1)] = 1
  nomov = c(0, nomov, 0)
  s1 = which(diff(nomov) == 1)
  e1 = which(diff(nomov) == -1)
  bedblock = which((e1 - s1) > ((60/epoch) * 60 * 1))

  if (length(bedblock) > 0) {
    s2 = s1[bedblock]
    e2 = e1[bedblock]
    if (e2[length(e2)] > length(xangle)) e2[length(e2)] = length(xangle)
    for (j in 1:length(s2)) {
      inbedtime[s2[j]:e2[j]] = 1
    }
    legangle[which(inbedtime == 1 & abs(xangle) > 35)] = 1
    # out of bed
    outofbed = rep(0, length(inbedtime))
    outofbed[which(is.na(inbedtime) == TRUE)] = 1
    outofbed = c(0, outofbed,0)
    s3 = which(diff(outofbed) == 1)
    e3 = which(diff(outofbed) == -1)
    outofbedblock = which((e3 - s3) < ((60/epoch) * 30 * 1))
    if (length(outofbedblock) > 0) {
      s4 = s3[outofbedblock]
      e4 = e3[outofbedblock]
      if (e4[length(e4)] > length(inbedtime)) e4[length(e4)] = length(inbedtime)
      if (length(s4) > 0) {
        for (j in 1:length(s4)) {
          inbedtime[s4[j]:e4[j]] = 1
        }
        legangle[which(inbedtime == 1 & abs(xangle) > 35)] = 1
      }
    }
    if (length(inbedtime) == (length(x) + 1)) inbedtime = inbedtime[1:(length(inbedtime) - 1)]

    inbedtime[inbedtime %in% NA] = 0
    s4 = which(diff(c(0, inbedtime, 0)) == 1)
    e4 = which(diff(c(0, inbedtime, 0)) == -1)
    #apply 75% rule for sleep/nonwear determination
    for (nw in 1:length(s4)) {
      non = length(which(legangle[s4[nw]:e4[nw]] == 1)) #determine length of non-wear within sleep duration
      sl = length(which(inbedtime[s4[nw]:e4[nw]] == 1)) #determine length of sleep duration
      if (non/sl < 0.2) inbedtime[s4[nw]:e4[nw]] = 0 #apply 75% rule
    }
    s5 = which(diff(c(0, inbedtime, 0)) == 1)
    e5 = which(diff(c(0, inbedtime, 0)) == -1)
    if (length(s5) > 0) { #check if there are actually any sleep periods
      for (zz in 1:length(s5)) ts$sleep[s5[zz]:e5[zz]] = 1
    }
  }
  browser()
  a<-unique(ts$date)
  b<-1
  c<-2
  sss=2
  for(sss in 1: length(a))
  {
    if(c>length(a)){
      break
    }


    try(if(c<=length(a)){z<-which(ts$date==a[b]&ts$time>="12:00:00")
    z<-z[1]
    x<-which(ts$date==a[c]&ts$time<="12:00:00")
    x<-x[length(x)]
    if(length(x)==0){break}
    x<-x-1
    ga<-ts[z:x,]
    inbedtime2 = rep(0, nrow(ga))
    inbedtime2[which(ga$sleep == 1)] = 1
    s5 = which(diff(c(0, inbedtime2, 0)) == 1)
    e5 = which(diff(c(0, inbedtime2, 0)) == -1)
    inbeddurations = e5 - s5
    longestinbed = which(inbeddurations == max(inbeddurations))
    lightsout = s5[longestinbed] - 1
    lightson = e5[longestinbed] - 1

    d1<-ga$date[lightsout]
    t1<-ga$time[lightsout]
    e<-which(ts$date%in%d1 &ts$time>=t1)
    e<-e[1]
    d1<-ga$date[lightson]
    t1<-ga$time[lightson]
    e1<-which(ts$date%in%d1 &ts$time<=t1)
    e1<-e1[length(e1)]
    for(zz in 1:length(e)){
      ts$sleep[e[zz]:e1[zz]]<-"sleep"
    }


    },silent=T)
    b<-b+1
    c<-c+1

    cat(paste("completed sleep for day:",sss,"\r",sep=" "))

  }

}
