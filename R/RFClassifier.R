#' Title
#'
#' @param raw
#' @param Fs
#' @param ID
#' @param mypath
#' @param sleep
#' @param win
#' @param rfmodel
#' @param Classifier
#' @param start.time
#'
#' @return
#' @export
#'
#' @examples
classifier_Preschool<-function(raw,Fs,ID,mypath,sleep,win,rfmodel,Classifier,start.time){

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

  NR = (60/5)*60*24*30
  hold<- matrix(9999,NR,29) #matrix to hold one month of data at 5s windows
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
    if(LD<Fs*15){#need a minimum of 1 window length to extract features
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
    acc2<- round_df(feature.extraction(acc,domain=3, vector.mag =2, window=win,overlap=0,freq=Fs,
                                       upper=5,lower=.25),3)

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

  cut = which(hold[, 1] == 9999)
  hold =hold[-cut, ]
  if(Classifier%in%"Preschool Hip Random Forest Free Living Lag-Lead"|Classifier%in%"Preschool Wrist Random Forest Free Living Lag-Lead"){
    cat("\nExtracting lag-lead features")
    lagsd1<-dplyr::lag(hold[,2],default = 0)
    lagsd2<-dplyr::lag(hold[,2],default = 0,n=2)
    leadsd1<-dplyr::lead(hold[,2],default = 0)
    leadsd2<-dplyr::lead(hold[,2],default = 0,n=2)
    laglead<-cbind(lagsd1,lagsd2,leadsd1,leadsd2)
    combsd<-apply(cbind(laglead,hold[,2]),1,sd)
    hold<-cbind(hold[,1:26],laglead,combsd,hold[,27:29])
  }
  b<- Sys.time()-a
  cat(paste("\nfeature extraction completed:",format(b,digits=2),"\n"))

  time<-start.time + win*(0:(nrow(hold)-1))#################### FIX WINDOW SIZE AND OVERLAP ACCORDINGLY
  class(time) = c('POSIXt','POSIXct')############################### THESE TWO LINES ARE IMPORTANT TO CONVERT NUMERIC STRING TO DATE/TIME
  cat("completed timestamp")
  time2<- strptime(time,"%Y-%m-%d %H:%M:%OS")
  time2[is.na(time2)] = strptime(time[is.na(time2)],"%Y-%m-%d")
  date<- format(time2, "%Y-%m-%d")
  time3<- paste0(" ",format(time, "%H:%M:%OS"))
  raw<- data.frame(subject=ID,date=date,time=time3,hold)
  ###########################scoring file
  cat("\nClassifying Activity\n")
  if(Classifier%in%"Preschool Hip Random Forest Free Living Lag-Lead"|Classifier%in%"Preschool Wrist Random Forest Free Living Lag-Lead"){
    b<-rfmodel$xNames
    colnames(raw)<-c("subject","date","time",b[1:14],"sum","mad",b[15:20],"Entropy_0.25.5hz",
                     b[21:28],"enmo","tilt","nonwear")
  }
  if(Classifier%in%"Preschool Wrist Random Forest Free Living"|Classifier%in%"Preschool Hip Random Forest Free Living"){
    b<-rfmodel$coefnames
    colnames(raw)<-c("subject","date","time",b[1:14],"sum","mad",b[15:20],"Entropy_0.25.5hz",
                     b[21:23],"enmo","tilt","nonwear")
  }
  if(Classifier%in%"School age Wrist Random Forest"){
    a<-data.frame(rfmodel$forest$xlevels)
    b<-colnames(a)
    colnames(raw)<-c("subject","date","time",b[12],b[22],b[2],b[15],b[16],b[17],b[18],b[19],b[21],b[9],
                     b[11],b[14],b[24],b[13],b[23],"mad",b[20],b[1],b[10],b[8],
                     b[6],b[7],"entropy",b[3],b[4],b[5],"enmo","tilt","nonwear")}
  if(Classifier%in%"School age Hip Random Forest"){
    b<-rfmodel$coefnames
    colnames(raw)<-c("subject","date","time",b[1:22],"entropy",b[23:25],"enmo","tilt","nonwear")
  }

  raw<-do.call(data.frame,lapply(raw, function(x) replace(x, is.infinite(x),NA)))
  raw[is.na(raw)] <- 0
  class<-predict(rfmodel,raw)
  if(Classifier%in%"Preschool Hip Random Forest Free Living"){
    levels(class)<-c("2","3","5","1","4")
    class<-as.character(class)
  }
  if(Classifier%in%"School age Wrist Random Forest"){
    levels(class)<-c("1","2","4","5","3")
    class<-as.character(class)
  }
  raw<- cbind(raw,class)
  raw$class<-as.numeric(raw$class)
  raw$Activity_orig<-raw$class
  ########################sleep detection
  if(sleep==T){
    cat("\nDetecting Sleep Window\n")

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
      sleepw<-inbed(ga$s.anglez,outofbedsize = 30,ws3=5,bedblocksize = 30,k=60)

      if(sleepw$lightsout[1]==0){sleepw$lightsout[1]<-1}

      d1<-ga$s.date[sleepw$lightsout]
      t1<-ga$s.t2[sleepw$lightsout]
      e<-which(raw$date%in%d1 &raw$time>=t1)
      e<-e[1]
      d1<-ga$s.date[sleepw$lightson]
      t1<-ga$s.t2[sleepw$lightson]
      e1<-which(raw$date%in%d1 &raw$time<=t1)
      e1<-e1[length(e1)]
      for(zz in 1:length(e)){
        raw$sleep[e[zz]:e1[zz]]<-"s"
      }

      },silent=T)
      b<-b+1
      c<-c+1
      cat(paste("completed Sleep Window for day:",sss,"\r",sep=" "))

    }}
  else{
    raw$sleep<-0
    time<-strptime(raw$time, "%H:%M:%S")
    time<-format(time, "%H:%M:%S")
    raw$time<-time }

  ###########adjust sleep and nonwear based on preset 75% Decision Fusion Rule
  raw$nonwear_orig<-raw$nonwear
  hold<-ifelse(raw$sleep%in%"s",1,0)

  hold<-c(0,diff(hold))

  st<-which(hold==1)
  if(length(st)==0){st<-1}
  end<-which(hold==-1)
  end<-end-1
  if(length(st)>length(end)){end<-c(end,length(hold))}

  for(nw in 1:length(st)){
    non<-length(which(raw$nonwear[st[nw]:end[nw]]==1))
    sl<-length(which(raw$sleep[st[nw]:end[nw]]%in%"s"))
    if(!is.na(non/sl)){
      if(non/sl<.75){raw$nonwear[st[nw]:end[nw]]<-6}}

  }
  #######################
  e<-which(raw$nonwear==6)
  raw$class[e]<-6
  e<-which(raw$nonwear==1)
  raw$class[e]<-7

  if(sleep==T){
    cat("\nDetecting Sleep Periods\n")
    raw$sleep_windows_orig<-ifelse(raw$sleep%in%'s',1,0)
    raw$sleep_periods<-detect_sleep_periods(raw,win)
  }
  else{
    raw$sleep_windows_orig<-0
    raw$sleep_periods<-0
  }
  raw<-subset(raw,select=-c(nonwear,sleep))
  invisible(list(Activity = raw))
  ######################################################
}
