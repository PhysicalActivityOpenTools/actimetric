#' Title
#'
#' @param x
#' @param boutduration
#' @param boutcriter
#' @param closedbout
#' @param bout.metric
#' @param ws3
#'
#' @return
#' @export
#'
#'@importFrom data.table fread
#'
#' @examples
getbout<-function (x, boutduration, boutcriter = 0.8, closedbout = FALSE,
                   bout.metric = 1, ws3 = 5)
{
  p = which(x == 1)
  if (bout.metric == 1) {
    xt = x
    boutcount = rep(0, length(x))
    jmvpa = 1
    Lx = length(x)
    while (jmvpa <= length(p)) {
      endi = p[jmvpa] + boutduration
      if (endi <= Lx) {
        if (sum(x[p[jmvpa]:endi]) > (boutduration * boutcriter)) {
          while (sum(x[p[jmvpa]:endi]) > ((endi - p[jmvpa]) *
                                          boutcriter) & endi <= Lx) {
            endi = endi + 1
          }
          select = p[jmvpa:max(which(p < endi))]
          jump = length(select)
          xt[select] = 2
          boutcount[p[jmvpa]:p[max(which(p < endi))]] = 1
        }
        else {
          jump = 1
          x[p[jmvpa]] = 0
        }
      }
      else {
        jump = 1
        if (length(p) > 1 & jmvpa > 2) {
          x[p[jmvpa]] = x[p[jmvpa - 1]]
        }
      }
      jmvpa = jmvpa + jump
    }
    x[which(xt == 2)] = 1
    if (length(which(xt == 1)) > 0)
      x[which(xt == 1)] = 0
    if (closedbout == TRUE) {
      x = boutcount
    }
  }

  x[which(xt != 2)] = 0
  x[which(xt == 2)] = 1
  boutcount = x

  invisible(list(x = x, boutcount = boutcount))
}
slide<-function(x, width, by = NULL, FUN = NULL, ...)
{
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width

  lenX <- length(x)
  QUT1 <- seq(1, lenX - width + 1, by = by)
  QUT2 <- lapply(QUT1, function(x) x:(x + width - 1))

  QUT3 <- lapply(QUT2, function(a) FUN(x[a], ...))
  QUT4 <- do.call(rbind,QUT3)
  return(QUT4)
}
inbed = function(angle, k = 60, perc = 0.1, inbedthreshold = 15,
                 bedblocksize = 30, outofbedsize = 60, ws3 = 5) {
  medabsdi = function(angle) {
    angvar = stats::median(abs(diff(angle)))
    return(angvar)
  }
  x = slide(angle, width = k, FUN=medabsdi,by=1)
  nomov = rep(0, length(x))
  inbedtime = rep(NA, length(x))
  pp = quantile(x, probs = c(perc)) * inbedthreshold
  if (pp == 0)
    pp = 7
  nomov[which(x < pp)] = 1
  nomov = c(0, nomov, 0)
  s1 = which(diff(nomov) == 1)
  e1 = which(diff(nomov) == -1)
  bedblock = which((e1 - s1) > ((60/ws3) * bedblocksize *
                                  1))
  if (length(bedblock) > 0) {
    s2 = s1[bedblock]
    e2 = e1[bedblock]
    for (j in 1:length(s2)) {
      inbedtime[s2[j]:e2[j]] = 1
    }
    outofbed = rep(0, length(inbedtime))
    outofbed[which(is.na(inbedtime) == TRUE)] = 1
    outofbed = c(0, outofbed, 0)
    s3 = which(diff(outofbed) == 1)
    e3 = which(diff(outofbed) == -1)
    outofbedblock = which((e3 - s3) < ((60/ws3) * outofbedsize *
                                         1))
    if (length(outofbedblock) > 0) {
      s4 = s3[outofbedblock]
      e4 = e3[outofbedblock]
      if (length(s4) > 0) {
        for (j in 1:length(s4)) {
          inbedtime[s4[j]:e4[j]] = 1
        }
      }
    }
    if (length(inbedtime) == (length(x) + 1))
      inbedtime = inbedtime[1:(length(inbedtime) -
                                 1)]
    inbedtime2 = rep(1, length(inbedtime))
    inbedtime2[which(is.na(inbedtime) == TRUE)] = 0
    s5 = which(diff(c(0, inbedtime2, 0)) == 1)
    e5 = which(diff(c(0, inbedtime2, 0)) == -1)
    inbeddurations = e5 - s5
    #longestinbed = which(inbeddurations == max(inbeddurations))
    lightsout = s5 -1
    lightson = e5 - 1
  }
  else {
    lightson = c()
    lightsout = c()
    tib.threshold = c()
  }
  tib.threshold = pp
  invisible(list(lightsout = lightsout, lightson = lightson,
                 tib.threshold = tib.threshold))
}

summary_general3<-function(ag,win=5){
  day<-weekdays(as.Date(ag$date)) #this will be used when wanting to summarise data by day of week
  ag<-cbind(ag,day)
  avday<-ag
  allday<-ag
  ag<-ag[,c(1:3,5,30,32,33)]

  a<-unique(ag$date)
  mat<-matrix("",length(a),19)
  see<-list()

  for(i in 1:length(a)){
    final<-ag[ag$date%in%a[i]]
    sed<-round(as.numeric(length(which(final$intensity==1)))*win/60,2)
    light<-round(as.numeric(length(which(final$intensity==2)))*win/60,2)
    moderate<-round(as.numeric(length(which(final$intensity==3)))*win/60,2)
    vigorous<-round(as.numeric(length(which(final$intensity==4)))*win/60,2)
    ii=i+1
    if(ii<=length(a)){
      final4<-ag[ag$date==a[i]|ag$date==a[ii] ,]
      time2<- strptime(paste(final4$date, final4$time,sep = ""),"%Y-%m-%d %H:%M:%S")
      final4$time3<- format(time2, "%H:%M:%S")
      z<-which(final4$date==a[i] & final4$time3>="12:00:00")
      z<-z[1]
      x<-which(final4$date==a[ii] & final4$time3<="12:00:00")
      x<-x[length(x)]
      x<-x-1
      final4<-final4[z:x,]
      e<-as.numeric(length(which(final4$intensity==6)))
      sleep<-round(e*win/60,2)
      e<-which(final4$intensity==6)
      st.sleep<-final4$time[e[1]]
      end.sleep<-final4$time[e[length(e)]]
      st.day<-final4$date[e[1]]
      end.day<-final4$date[e[length(e)]]
      final4<-final4[e]
      matt<-ifelse(final4$enmo*1000>=44.8,'r','l')
      post<-which(final4$std_mag_X_Y_Z>.013)
      q1<-which(diff(post)>60)
      nap<-rep(0,nrow(final4))
      if(length(q1)>0){
        for(iii in 1:length(q1)){
          nap[post[q1[iii]]:post[q1[iii]+1]]<-1
        }
      }
      e<-which(nap==1)
      matt[e]<-'m'
      motionless<-round(as.numeric(length(which(matt%in%"m")))*win/60,2)
      light.s<-round(as.numeric(length(which(matt%in%"l")))*win/60,2)
      restless<-round(as.numeric(length(which(matt%in%"r")))*win/60,2)


    }
    if(ii>length(a)){
      sleep<-NA
      st.sleep<-NA
      end.sleep<-NA
      st.day<-NA
      end.day<-NA
      motionless<-NA
      light.s<-NA
      restless<-NA
    }
    nw<-round(as.numeric(length(which(final$intensity==7)))*win/60,2)

    boutduration = 10 * (60/win) #per 10 minutes

    rr1 = matrix(0,length(final$intensity),1)

    p = which(final$intensity==3|final$intensity==4); rr1[p] = 1
    gbo<-getbout(rr1,boutduration = boutduration)
    e<-as.numeric(length(which(gbo$x == 1)))
    mvpa<-round(e*win/60,2)

    rr1 = matrix(0,length(final$intensity),1)

    p = which(final$intensity==1); rr1[p] = 1
    gbo<-getbout(rr1,boutduration = boutduration*6,boutcriter = .9)
    e<-as.numeric(length(which(gbo$x == 1)))
    sedb<-round(e*win/60,2)


    see[[i]]<-list(sed = sed,light = light,moderate = moderate,vigorous = vigorous,sleep = sleep, nw = nw,date = final$date[1], day = final$day[1],ID = final$subject[1],mvpa = mvpa, sedb = sedb,
                   start = st.sleep, end = end.sleep, start.day = st.day, end.day = end.day,
                   motionless = motionless, light.s = light.s, restless = restless)

  }


  mat[1,1]<-"ID";mat[1,2]<-"Date";mat[1,3]<-"Day";mat[1,4]<-"Weekend";mat[1,5]<-"Sed";mat[1,6]<-"Light";mat[1,7]<-"mod";mat[1,8]<-"vig";mat[1,9]<-"Sleep";mat[1,10]<-"NW"
  mat[1,11]<-"mvpa.b";mat[1,12]<-"sed.b";mat[1,13]<-"sleep.start";mat[1,14]<-"sleep.end";mat[1,15]<-"st.day";mat[1,16]<-"end.day"
  mat[1,17]<-"motionless";mat[1,18]<-"light.s";mat[1,19]<-"restless"
  z<-0
  for(i in 1:length(a)){
    z<-z+1
    aa<-see[[i]]
    mat[z,1]<-aa$ID;mat[z,2]<-aa$date;mat[z,3]<-aa$day;mat[z,4]<-aa$day;mat[z,5]<-aa$sed;mat[z,6]<-aa$light;mat[z,7]<-aa$moderate;mat[z,8]<-aa$vigorous;mat[z,9]<-aa$sleep;mat[z,10]<-aa$nw
    mat[z,4]<-ifelse(mat[z,4]%in%c("Saturday","Sunday"),0,1)
    mat[z,11]<-aa$mvpa;mat[z,12]<-aa$sedb;mat[z,13]<-aa$start;mat[z,14]<-aa$start.day
    if(length(aa$end)==0){aa$end<-NA}
    mat[z,15]<-aa$end
    if(length(aa$end.day)==0){aa$end.day<-NA}
    mat[z,16]<-aa$end.day
    mat[z,17]<-aa$motionless;mat[z,18]<-aa$light.s;mat[z,19]<-aa$restless

  }
  ##################################
  a<-unique(avday$date)
  en<-matrix(9999,(60/5)*60*24,length(a))
  tilt<-matrix(9999,(60/5)*60*24,length(a))

  for(i in 1:length(a)){
    ag2<-avday[avday$date%in%a[i],]
    if(nrow(ag2)==(60/5)*60*24){
      en[,i]<-ag2$enmo
      tilt[,i]<-ag2$tilt

    }

  }
  intensity<-rep(0,nrow(tilt))
  tilt<-tilt[,!(tilt[1,]==9999)]
  en<-en[,!(en[1,]==9999)]
  en<-as.matrix(en)
  tilt<-as.matrix(tilt)
  tilt<-apply(tilt,1,mean)
  sleep<-inbed(tilt)
  if(sleep$lightsout[1]==0){sleep$lightsout[1]<-1}
  en<-apply(en,1,mean)
  e<-which(en*1000<=44.8)
  intensity[e]<-"black"
  e<-which(en*1000>44.8 & en*1000<100.6)
  intensity[e]<-"skyblue"
  e<-which(en*1000>=100.6 & en*1000<428.8)
  intensity[e]<-"green"
  e<-which(en*1000>=428.8)
  intensity[e]<-"purple"

  for(i in 1:length(sleep$lightsout)){
    intensity[sleep$lightsout[i]:sleep$lightson[i]]<-"red"
  }
  time = 0 + 5*(0:(length(en)-1))
  st.day<-format(as.POSIXct('0001-01-01 00:00:00') + time, "%I:%M:%S %p")

  avday<-data.frame(st.day,en,tilt,intensity)



  return(list(mat = mat, avday = avday, allday = allday))
}

# #ag<-Sys.glob("file_path_omitted")
# ag<-Sys.glob("file_path_omitted")
# for(i in 1:length(ag)){
#   final<-data.table::fread(ag[i])
#   bb<-summary(final)
#   colnames(bb$mat)<-c("File","Date","Day","Weekday","Sed","Light","Mod","Vig","Sleep","Non-wear","mvpa.b","sed.b",
#                       "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless")
#   ID<-unlist(strsplit(ag[i],split='\\',fixed=TRUE))
#   ID<-ID[length(ID)]
#   ID<-unlist(strsplit(ID,split='.',fixed=TRUE))
#   ID<-ID[1]
#   bb$mat[,1]<-ID
#   bb$allday$subject<-ID
#   #save(bb,file=paste("file_path_omitted",ID ,".RDATA",sep=""))
#   save(bb,file=paste("file_path_omitted",ID ,".RDATA",sep=""))
#
# }


