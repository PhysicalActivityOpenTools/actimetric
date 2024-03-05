#' Title
#'
#' @param raw
#' @param Fs
#' @param ID
#' @param mypath
#' @param win
#' @param sleep
#' @param rfmodel
#' @param Classifier
#' @param start.time
#'
#' @return
#' @export
#'
#' @examples
classifier_thigh<-function(raw,Fs,ID,mypath,win,sleep,rfmodel=rfmodel,Classifier=Classifier,start.time){


  #bf = signal::butter(n=4, c(5/(Fs/2)), type = c("low")) #4th order BF, low pass, 5Hz cutoff see skotte 2014
  #source(paste(mypath,"/shannon.entropy.R",sep = ""))
  #source(paste(mypath,"/extract.features.R",sep = ""))





  NR = (60/10)*60*24*30 #setting value equal to one month of data at 10s windows
  hold<- matrix(9999,NR,17) #matrix to hold one month of data at 10s windows
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
    if(LD<Fs*10){#need a minimum of 1 window length to extract features
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
    acc2<-matrix(9999,dim(ax)[2],16)
    while(ii <=dim(ax)[2]){

      ################################################
      # x = signal::filter(bf,ax[,ii] )
      # y = signal::filter(bf, ay[,ii])
      # z = signal::filter(bf, az[,ii])
      x =  ax[,ii]
      y =  ay[,ii]
      z =  az[,ii]
      bfvm<-sqrt((y^2)+(x^2)+(z^2))
      q<-as.numeric(quantile(bfvm,p=.5))####median crossing calculation start
      bb<-ifelse(bfvm>q,1,0)
      d<-diff(bb)
      e<-which(d>0)
      e<-as.numeric(length(e))

      bb<-ifelse(bfvm<q,1,0)
      d<-diff(bb)
      f<-which(d>0)
      f<-as.numeric(length(f))
      medianvm<-f+e #####median crossing calcuation end

      inc.y<- mean(acos(y/bfvm)*(180/pi))
      inc.x<- mean(acos(x/bfvm)*(180/pi))
      inc.z<- mean(acos(z/bfvm)*(180/pi))

      fb.y<- mean(-asin(y/bfvm)*(180/pi))
      fb.x<- mean(-asin(x/bfvm)*(180/pi))
      fb.z<- mean(-asin(z/bfvm)*(180/pi))

      bfsd.y<-sd(y)
      bfsd.x<-sd(x)
      bfsd.z<-sd(z)
      bfsd<-sd(bfvm)
      bfvm<-mean(bfvm)

      ############## steps
      q<-as.numeric(quantile(x,p=.5))####median crossing calculation start
      bb<-ifelse(x>q,1,0)
      d<-diff(bb)
      e<-which(d>0)
      e<-as.numeric(length(e))

      bb<-ifelse(x<q,1,0)
      d<-diff(bb)
      f<-which(d>0)
      f<-as.numeric(length(f))
      medianx<-f+e #####median crossing calcuation end

      q<-as.numeric(quantile(y,p=.5))####median crossing calculation start
      bb<-ifelse(y>q,1,0)
      d<-diff(bb)
      e<-which(d>0)
      e<-as.numeric(length(e))

      bb<-ifelse(y<q,1,0)
      d<-diff(bb)
      f<-which(d>0)
      f<-as.numeric(length(f))
      mediany<-f+e #####median crossing calcuation end

      q<-as.numeric(quantile(z,p=.5))####median crossing calculation start
      bb<-ifelse(z>q,1,0)
      d<-diff(bb)
      e<-which(d>0)
      e<-as.numeric(length(e))

      bb<-ifelse(z<q,1,0)
      d<-diff(bb)
      f<-which(d>0)
      f<-as.numeric(length(f))
      medianz<-f+e #####median crossing calcuation end
      ##################################################

      vm<-sqrt((ax[,ii]^2)+(ay[,ii]^2)+(az[,ii]^2))
      Enmo<- vm-1
      Enmo[Enmo<0]<- 0
      Enmo<-mean(Enmo)
      # wx<-extract.features(ax[,ii],wind=win*Fs,SampFreq = Fs)
      # wy<-extract.features(ay[,ii],wind=win*Fs,SampFreq = Fs)
      # wz<-extract.features(az[,ii],wind=win*Fs,SampFreq = Fs)
      # w<-as.matrix(cbind(wx,wy,wz))
      #w<-c(w,Enmo,bfvm,bfsd.y,bfsd.x,bfsd.z,inc.y,inc.x,inc.z,fb.y,fb.x,fb.z)
      w<-c(bfvm,bfsd,bfsd.y,bfsd.x,bfsd.z,inc.y,inc.x,inc.z,fb.y,fb.x,fb.z,medianx,mediany,medianz,medianvm,Enmo)
      if(length(w)<16){
        s<-16-length(w)
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
      e<-which(!hold[, 5] == 9999)
      nw<-rep(hold[(e[length(e)]-1),5],nrow(acc2))
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
  cat("completed timestamp\n")
  time2<- strptime(time,"%Y-%m-%d %H:%M:%OS")
  time2[is.na(time2)] = strptime(time[is.na(time2)],"%Y-%m-%d")
  date<- format(time2, "%Y-%m-%d")
  time3<- paste0(" ",format(time, "%H:%M:%OS"))
  #if(length(time)<nrow(hold)){hold<-hold[1:length(time),]} 2020/12/26 Not needed with new timestamp approach
  raw<- data.frame(subject=ID,date=date,time=time3,hold)
  ###########################scoring file

  b<-c("bfvm","bfsd","bfsd.y","bfsd.x","bfsd.z","inc.y","inc.x","inc.z","fb.y","fb.x","fb.z","median.x",
       'median.y','median.z','median.vm')
  colnames(raw)<-c("subject","date","time",c(b),"enmo","nonwear")
  ################    Implement Decision Tree
  raw$class<-NA
  raw$post<-NA
  thresh<-NA
  dat<-unique(raw$date)
  for(dit in 1 :length(dat)){
    temp<-raw[raw$date%in%dat[dit],]
    cat(paste("Classifying day", dit, "out of", length(dat),"\r",sep = ' '))

    # post<-which(abs(temp$fb.z)<45)
    # if(length(post)<1){ post<-"x1a"}
    #
    # else if(length(post)>=1){
    #   if(abs(base::summary(temp$fb.y[post])[3])<20){
    #     if(sign(base::summary(temp$fb.x)[3])==1){ post<-"x1a"}
    #     if(sign(base::summary(temp$fb.x)[3])==-1){
    #       post<-"x1b"
    #       temp$inc.y<-180-temp$inc.y; temp$inc.x<-180-temp$inc.x
    #       temp$fb.y<-(-1)*temp$fb.y; temp$fb.x<-(-1)*temp$fb.x
    #       }
    #   }
    #   else if(abs(base::summary(temp$fb.y[post])[3])>=20){
    #     if(sign(base::summary(temp$fb.y)[3])==1){
    #       post<-"x2a"
    #       temp <- transform(temp, inc.y = inc.x, inc.x = inc.y)
    #       temp <- transform(temp, fb.y = fb.x, fb.x = fb.y)
    #     }
    #     if(sign(base::summary(temp$fb.y)[3])==-1){
    #       post<-"x2b"
    #       temp <- transform(temp, inc.y = inc.x, inc.x = inc.y)
    #       temp <- transform(temp, fb.y = fb.x, fb.x = fb.y)
    #       temp$inc.y<-180-temp$inc.y; temp$inc.x<-180-temp$inc.x
    #       temp$fb.y<-(-1)*temp$fb.y; temp$fb.x<-(-1)*temp$fb.x
    #       }
    #   }
    # }
    post<-"x1a"
    if(post%in%"x1a"|post%in%"x1b"|post%in%"x2a"|post%in%"x2b"){
      inds<-which(temp$bfsd.x<=0.1 & temp$inc.x<135)
      temp$class[inds]<-"sit"
      e<-apply(temp[,6:8],1,max)
      inds<-which( e>0.1 & temp$bfsd.x<=0.1 & abs(temp$inc.x)>=135)
      temp$class[inds]<-"move"
      inds<-which( e<=0.1 & temp$bfsd.x<=0.1 & temp$inc.x>=135)
      temp$class[inds]<-"stand"

      ee<-which(temp$class%in%NA)
      inds<-which(temp$bfsd.x[ee]>0.1 & temp$fb.z[ee]>24)
      temp$class[ee[inds]]<-"cycle"

      inds<-which(temp$bfsd.x[ee]>0.1 & temp$bfsd.x[ee]>0.72 & temp$fb.z[ee]<24)
      temp$class[ee[inds]]<-"run"
      inds<-which(temp$bfsd.x[ee]>0.1 & temp$bfsd.x[ee]<0.72 & temp$fb.z[ee]<24)
      temp$class[ee[inds]]<-"walk"

      e<-which(temp$class%in%"sit")
      if(length(e)>0){
        thresh<-as.numeric(abs(base::summary(temp$fb.z[e & temp$fb.z<5 & temp$fb.z>-5])[3])+4.5)
        inds<-which(temp$class%in%c('walk','run') & temp$fb.z>thresh)
        temp$class[inds]<-"stairs"
      }
      if(length(e)==0 & !thresh%in%NA){
        inds<-which(temp$class%in%c('walk','run') & temp$fb.z>thresh)
        temp$class[inds]<-"stairs"
      }
      temp$post<-post
      raw[raw$date%in%dat[dit],]<-temp

    }
    #########################


  }

  ########################sleep detection
  cat("\n")
  if(sleep==T){

    cat("\nDetecting Sleep\n")
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    raw$sleep<-NA
    time<-strptime(raw$time, "%H:%M:%S")
    time<-format(time, "%H:%M:%S")
    raw$time<-time

    x = slide(raw$bfsd.x, width = (60/win)*5, FUN=Median,by=1)
    x2 = slide(raw$inc.x, width = (60/win)*5, FUN=Median,by=1)
    x2<-which(x<0.1 & x2 <135)
    x<-rep(NA,length(x))
    x[x2]<-'sit'
    xangle = slide(raw$fb.y, width = (60/win)*5, FUN=Median,by=1)


    nomov = rep(0, length(x))
    inbedtime = rep(NA, length(x))
    legangle =  rep(0, length(x))
    nomov[which(x%in%"sit")] = 1
    nomov = c(0, nomov, 0)
    s1 = which(diff(nomov) == 1)
    e1 = which(diff(nomov) == -1)
    bedblock = which((e1 - s1) > ((60/win) * 60 *
                                    1))

    if (length(bedblock) > 0) {
      s2 = s1[bedblock]
      e2 = e1[bedblock]
      if(e2[length(e2)]>length(xangle)){e2[length(e2)]<-length(xangle)}


      for (j in 1:length(s2)) {
        inbedtime[s2[j]:e2[j]] = 1
      }



      legangle[which(inbedtime==1 & abs(xangle)>35)]<-1


      outofbed = rep(0, length(inbedtime))
      outofbed[which(is.na(inbedtime) == TRUE)] = 1
      outofbed = c(0, outofbed,0)
      s3 = which(diff(outofbed) == 1)
      e3 = which(diff(outofbed) == -1)
      outofbedblock = which((e3 - s3) < ((60/win) * 30 *
                                           1))
      if (length(outofbedblock) > 0) {
        s4 = s3[outofbedblock]
        e4 = e3[outofbedblock]
        if(e4[length(e4)]>length(inbedtime)){e4[length(e4)]<-length(inbedtime)}
        if (length(s4) > 0) {
          for (j in 1:length(s4)) {
            inbedtime[s4[j]:e4[j]] = 1
          }
          legangle[which(inbedtime==1 & abs(xangle)>35)]<-1

        }
      }
      if (length(inbedtime) == (length(x) + 1))
        inbedtime = inbedtime[1:(length(inbedtime) -1)]

      inbedtime[inbedtime%in%NA]<-0
      s4 = which(diff(c(0, inbedtime, 0)) == 1)
      e4 = which(diff(c(0, inbedtime, 0)) == -1)

      for(nw in 1:length(s4)){
        non<-length(which(legangle[s4[nw]:e4[nw]]==1)) #determine length of non-wear within sleep duration
        sl<-length(which(inbedtime[s4[nw]:e4[nw]]==1)) #determine length of sleep duration
        if(non/sl<.2){inbedtime[s4[nw]:e4[nw]]<-0} #apply 75% rule

      }

      s5 = which(diff(c(0, inbedtime, 0)) == 1)
      e5 = which(diff(c(0, inbedtime, 0)) == -1)

      if(length(s5)>0){ #check if there are actually any sleep periods
        for(zz in 1:length(s5)){

          raw$sleep[s5[zz]:e5[zz]]<-"s"

        }
      }
    }
    a<-unique(raw$date)
    b<-1
    c<-2
    sss=2
    for(sss in 1: length(a))
    {
      if(c>length(a)){
        break
      }


      try(if(c<=length(a)){z<-which(raw$date==a[b]&raw$time>="12:00:00")
      z<-z[1]
      x<-which(raw$date==a[c]&raw$time<="12:00:00")
      x<-x[length(x)]
      if(length(x)==0){break}
      x<-x-1
      ga<-raw[z:x,]
      inbedtime2 = rep(0, nrow(ga))
      inbedtime2[which(ga$sleep%in%"s")] = 1
      s5 = which(diff(c(0, inbedtime2, 0)) == 1)
      e5 = which(diff(c(0, inbedtime2, 0)) == -1)
      inbeddurations = e5 - s5
      longestinbed = which(inbeddurations == max(inbeddurations))
      lightsout = s5[longestinbed] - 1
      lightson = e5[longestinbed] - 1

      d1<-ga$date[lightsout]
      t1<-ga$time[lightsout]
      e<-which(raw$date%in%d1 &raw$time>=t1)
      e<-e[1]
      d1<-ga$date[lightson]
      t1<-ga$time[lightson]
      e1<-which(raw$date%in%d1 &raw$time<=t1)
      e1<-e1[length(e1)]
      for(zz in 1:length(e)){
        raw$sleep[e[zz]:e1[zz]]<-"sleep"
      }


      },silent=T)
      b<-b+1
      c<-c+1

      cat(paste("completed sleep for day:",sss,"\r",sep=" "))

    }
  } else {
    raw$sleep<-0
    time<-strptime(raw$time, "%H:%M:%S")
    time<-format(time, "%H:%M:%S")
    raw$time<-time }
  ###########adjust sleep and nonwear based on preset 75% Decision Fusion Rule
  hold<-ifelse(raw$sleep%in%c("s","sleep"),1,0)
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
    sl<-length(which(raw$sleep[st[nw]:end[nw]]%in%c("s","sleep")))
    if(!is.na(non/sl)){
      if(non/sl<.75){raw$nonwear[st[nw]:end[nw]]<-6}}

  }


  #######################
  raw$class<-as.character(raw$class)
  invisible(list(Activity = raw))

  ######################################################
}
