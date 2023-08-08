#' Title
#'
#' @param raw
#' @param Fs
#' @param ID
#' @param mypath
#' @param win
#' @param sleep
#' @param start.time
#'
#' @return
#' @export
#'
#' @examples
EnmoClassifier<-function(raw,Fs,ID,mypath,win,sleep,start.time){

  ###########################sleep variables
  if(sleep==T){
    s.anglez = (atan(raw[,3]/ (sqrt(raw[,1]^2 + raw[,2]^2)))) / (pi/180)
    s.anglez<- slide(s.anglez,width=5*Fs,FUN=mean)
    s.t2<-start.time + 5*(0:(length(s.anglez)-1))
    class(s.t2) = c('POSIXt','POSIXct')
    #s.t2<-.POSIXct(s.t2,tz='UTC')
    #s.t2<-.POSIXct(s.t2,tz=Sys.timezone())
    #class(s.t2) = c('POSIXt','POSIXct')
    s.time2<- strptime(s.t2,"%Y-%m-%d %H:%M:%OS")
    s.date<- format(s.time2, "%Y-%m-%d")
    s.t2<- format(s.time2, "%H:%M:%OS")

    anglez<-data.frame(s.t2,s.date,s.anglez)
  }
  ############################


  # start.time<-raw[,1]

  NR = (60/10)*60*24*30 #setting value equal to one month of data at 10s windows
  hold<- matrix(9999,NR,50) #matrix to hold one month of data at 10s windows
  start = increment = constant = (Fs*60*60*24) #variables used to read data in 24 hr increment
  LD<-2
  count<-1
  chunk<-1
  cat("\n")
  b<-round(dim(raw)[1]/Fs/3600,2)
  a<- Sys.time()
  while(LD>1){

    if(increment>nrow(raw)){increment<-nrow(raw);LDD<-1}
    if((1+(start *(chunk -1)))>nrow(raw)){break}
    acc = raw[(1+(start *(chunk -1))):(increment) ,]
    acc<-na.omit(acc) #this is needed for the last day
    LD<-nrow(acc)
    if(LD<Fs*win){#need a minimum of 1 window length to extract features
      break
    }
    if (chunk == 1) {
      hr<-round(((LD/Fs)/3600),2)
    }
    else {
      hr<-hr+round(((LD/Fs)/3600),2)
    }

    cat(paste("Extracting features for hours",round((1+(start *(chunk -1)))/Fs/3600,2),"to",hr,"out of",b,"\r", sep = " "))


    increment = increment+constant


    ax<-matrix(acc[,1],nrow = win * Fs,ncol = ceiling(dim(acc)[1]/(win*Fs)),byrow = F)
    ay<-matrix(acc[,2],nrow = win * Fs,ncol = ceiling(dim(acc)[1]/(win*Fs)),byrow = F)
    az<-matrix(acc[,3],nrow = win * Fs,ncol = ceiling(dim(acc)[1]/(win*Fs)),byrow = F)
    ii=1
    acc2<-matrix(9999,dim(ax)[2],49)
    while(ii <=dim(ax)[2]){

      vm<-sqrt((ax[,ii]^2)+(ay[,ii]^2)+(az[,ii]^2))
      Enmo<- vm-1
      Enmo[Enmo<0]<- 0
      Enmo<-mean(Enmo)
      wx<-matrix(0,1,16)
      wy<-matrix(0,1,16)
      wz<-matrix(0,1,16)
      w<-as.matrix(cbind(wx,wy,wz))
      w<-c(w,Enmo)
      if(length(w)<49){
        s<-49-length(w)
        s<-rep(0,s)
        w<-c(w,s)
      }
      acc2[ii,]<-w
      ii<-ii+1
    }


    if(nrow(acc)>=(Fs*60*60*1)){#need at least 1hr of data to calculate nonwear
      nw<-nonwear_vm(acc,Fs=Fs,window=win)
      #nw<-rep(nw,each=4) #need to adjust according to window size
      if(length(nw)<nrow(acc2)){
        z<-abs(as.numeric(length(nw)-nrow(acc2)))
        z<-rep(nw[length(nw)],z)
        nw<-c(nw,z)
      }
      if(length(nw)>nrow(acc2)){
        e<-nrow(acc2)
        nw<-nw[1:e]
      }}
    if(nrow(acc)<(Fs*60*60*1)){
      e<-which(!hold[, 29] == 9999)
      nw<-rep(hold[(e[length(e)]-1),29],nrow(acc2))
    }
    acc2<-cbind(acc2,nw)
    hold[count:(count - 1 + dim(acc2)[1]),]<-as.matrix(acc2) #putting features into matrix

    count = count + nrow(acc2)
    LD<-nrow(acc2)
    chunk=chunk+1


  }
  b<- Sys.time()-a
  cat("\n")
  cat(paste("feature extraction completed:",format(b,digits=2),"\n"))
  cut = which(hold[, 1] == 9999)
  hold =hold[-cut, ]
  time<-start.time + win*(0:(nrow(hold)-1))#################### FIX WINDOW SIZE AND OVERLAP ACCORDINGLY
  class(time) = c('POSIXt','POSIXct')############################### THESE TWO LINES ARE IMPORTANT TO CONVERT NUMERIC STRING TO DATE/TIME
  #time<-.POSIXct(time,tz='UTC')
  #class(time) = c('POSIXt','POSIXct')
  cat("completed timestamp")
  time2<- strptime(time,"%Y-%m-%d %H:%M:%OS")
  time2[is.na(time2)] = strptime(time[is.na(time2)],"%Y-%m-%d")
  date<- format(time2, "%Y-%m-%d")
  time3<- paste0(" ",format(time, "%H:%M:%OS"))
  #if(length(time3)<nrow(hold)){hold<-hold[1:length(time3),]} 2020/11/19 Not needed with new timestamp approach
  raw<- data.frame(subject=ID,date=date,time=time3,hold)
  ###########################scoring file
  a<-c(1:48)
  a<-as.character(a)
  colnames(raw)<-c("subject","date","time",c(a),"enmo","nonwear")
  class<-rep(0,nrow(raw))
  raw<- cbind(raw,class)
  ########################sleep detection

  if(sleep==T){
    cat("\nDetecting Sleep\n")

    raw$sleep<-NA
    time<-strptime(raw$time, "%H:%M:%S")
    time<-format(time, "%H:%M:%S")
    raw$time<-time
    s.t2<-strptime(anglez$s.t2, "%H:%M:%S")
    s.t2<-format(s.t2, "%H:%M:%S")
    anglez$s.t2<-s.t2
    a<-unique(anglez$s.date)
    b<-1
    c<-2

    for(sss in 1: length(a))
    {
      if(c>length(a)){
        break
      }
      try(if(c<=length(a)){z<-which(anglez$s.date==a[b]&anglez$s.t2>="12:00:00")
      z<-z[1]
      x<-which(anglez$s.date==a[c]&anglez$s.t2<="12:00:00")
      x<-x[length(x)]
      if(length(x)==0){break}
      x<-x-1
      ga<-anglez[z:x,]
      ga<-ga[is.na(ga$s.anglez)==FALSE,] #For last day when monitor is plugged in and acc is 0's
      sleep<-inbed(ga$s.anglez,outofbedsize = 30,ws3=5,bedblocksize = 30,k=60)

      if(sleep$lightsout[1]==0){sleep$lightsout[1]<-1}

      d1<-ga$s.date[sleep$lightsout]
      t1<-ga$s.t2[sleep$lightsout]
      e<-which(raw$date%in%d1 &raw$time>=t1)
      e<-e[1]
      d1<-ga$s.date[sleep$lightson]
      t1<-ga$s.t2[sleep$lightson]
      e1<-which(raw$date%in%d1 &raw$time<=t1)
      e1<-e1[length(e1)]
      for(zz in 1:length(e)){
        raw$sleep[e[zz]:e1[zz]]<-"s"
      }
##########nap
      if(is.numeric(sleep$naplightsout)){

        sleep$naplightsout[sleep$naplightsout==0]<-1

      for(nap in 1:length(sleep$naplightsout)){
        d1<-ga$s.date[sleep$naplightsout[nap]]
        t1<-ga$s.t2[sleep$naplightsout[nap]]
      e<-which(raw$date%in%d1 &raw$time>=t1)
      e<-e[1]
      d1<-ga$s.date[sleep$naplightson[nap]]
      t1<-ga$s.t2[sleep$naplightson[nap]]
      e1<-which(raw$date%in%d1 &raw$time<=t1)
      e1<-e1[length(e1)]
      for(zz in 1:length(e)){
        raw$sleep[e[zz]:e1[zz]]<-"n"
      }}}

##########
      },silent=T)
      b<-b+1
      c<-c+1
      cat(paste("completed sleep for day:",sss,"\r",sep=" "))

    }}
  else{
    raw$sleep<-0
    time<-strptime(raw$time, "%H:%M:%S")
    time<-format(time, "%H:%M:%S")
    raw$time<-time }
  ###########adjust sleep and nonwear based on preset 75% Decision Fusion Rule
  raw$nw_sleep_75<-raw$nonwear
  hold<-ifelse(raw$sleep%in%c("s","n"),1,0)
  if(hold[1]==1){
    hold[1]<-0
  }
  hold<-c(0,diff(hold))

  st<-which(hold==1)
  if(length(st)==0){st<-1}
  end<-which(hold==-1)
  end<-end-1
  if(length(st)>length(end)){end<-c(end,length(hold))}

  for(nw in 1:length(st)){
    non<-length(which(raw$nonwear[st[nw]:end[nw]]==1))
    sl<-length(which(raw$sleep[st[nw]:end[nw]]%in%c("s")))
    if(!is.na(non/sl)){
      if(non/sl<.75){raw$nw_sleep_75[st[nw]:end[nw]]<-6}}

  }
  for(nw in 1:length(st)){
    non<-length(which(raw$nonwear[st[nw]:end[nw]]==1))
    sl<-length(which(raw$sleep[st[nw]:end[nw]]%in%c("n")))
    if(!is.na(non/sl)){
      if(non/sl<.75){raw$nw_sleep_75[st[nw]:end[nw]]<-7}}

  }

  #######################
  raw$class<-as.character(raw$class)
  e<-which(raw$nw_sleep_75==6)
  raw$class[e]<-6
  e<-which(raw$nw_sleep_75==1)
  raw$class[e]<-7
  e<-which(raw$nw_sleep_75==7)
  raw$class[e]<-8
  #raw<-raw[,-c(53,55)]
  invisible(list(Activity = raw))

  ######################################################
}
