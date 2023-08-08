#' Title
#'
#' @param ag
#' @param win
#'
#' @return
#' @export
#'
#' @examples
summary_TrostAdult<-function(ag,win=10){
  day<-weekdays(as.Date(ag$date)) #this will be used when wanting to summarise data by day of week
  ag<-cbind(ag,day)
  allday<-ag
  #ag<-ag[,c(1:3,24:25,52,53,54,58)]


  a<-unique(ag$date)
  mat<-matrix("",length(a),42)
  see<-list()

  for(i in 1:length(a)){
    final<-ag[ag$date%in%a[i],]
    int.sed<-round(as.numeric(length(which(final$intensity==1)))*win/60,2)
    int.light<-round(as.numeric(length(which(final$intensity==2)))*win/60,2)
    int.moderate<-round(as.numeric(length(which(final$intensity==3)))*win/60,2)
    int.vigorous<-round(as.numeric(length(which(final$intensity==4)))*win/60,2)

    class.sed<-round(as.numeric(length(which(final$class%in%"1")))*win/60,2)
    class.stationaryplus<-round(as.numeric(length(which(final$class%in%"2")))*win/60,2)
    class.walk<-round(as.numeric(length(which(final$class%in%"3")))*win/60,2)
    class.run<-round(as.numeric(length(which(final$class%in%"4")))*win/60,2)

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
      final4<-final4[e,]
      e<-as.numeric(length(which(final4$sleep_periods==1)))
      sleepperiod<-round(e*win/60,2)
      matt<-ifelse(final4$enmo*1000>=63.3,'r','l')
      post<-which(final4$SD.Y>.013)
      q1<-which(diff(post)>((60/win)*5))
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
      sleepperiod<-NA
      st.sleep<-NA
      end.sleep<-NA
      st.day<-NA
      end.day<-NA
      motionless<-NA
      light.s<-NA
      restless<-NA
    }
    nw<-round(as.numeric(length(which(final$class==7)))*win/60,2)
    nw.mg<-round(mean(final$enmo[which(final$class==7)]*1000),5)
    nw.mgvm<-round(mean(final$Mean.Y[which(final$class==7)]),5)

    boutduration = 10 * (60/win) #per 10 minutes

    rr1 = matrix(0,length(final$intensity),1)

    p = which(final$class%in%"3"|final$class%in%"4"); rr1[p] = 1
    gbo<-getbout(rr1,boutduration = boutduration,ws3=win)
    e<-as.numeric(length(which(gbo$x == 1)))
    mvpa<-round(e*win/60,2)
    e<-c(0,gbo$boutcount)
    e<-diff(e)
    mvpabouts<-length(which(e==1))
    mvpaboutsavg<-round(mvpa/mvpabouts,2)
    st<-which(e==1)
    end<-which(e==-1)
    if(length(st)>length(end)){end<-c(end,nrow(final))} #In case monitoring period ends with MVPA bout
    if(length(end)>=1){end<-end-1}
    mvpadura<-end-st
    if(mvpabouts>=1){
      dura = which(mvpadura == max(mvpadura))
      durast = st[dura]
      duraend = end[dura]
      mvpamaxdura<-round(length(which(gbo$x[durast:duraend]==1))*win/60,2)
      mvpamaxdurast<-final$time[durast]
      mvpamaxduraend<-final$time[duraend]
    }
    if(!mvpabouts>=1){

      mvpamaxdura<-NA
      mvpamaxdurast<-NA
      mvpamaxduraend<-NA
    }
    if(mvpabouts>1){
      dura = which(mvpadura == min(mvpadura))
      mvpamindurast = st[dura]
      mvpaminduraend = end[dura]
      mvpamindura<-round(length(which(gbo$x[mvpamindurast:mvpaminduraend]==1))*win/60,2)
      mvpamindurast<-final$time[mvpamindurast]
      mvpaminduraend<-final$time[mvpaminduraend]
    }
    if(mvpabouts<=1){
      mvpamindurast<-NA
      mvpaminduraend<-NA
      mvpamindura<-NA
    }

    rr1 = matrix(0,length(final$intensity),1)

    p = which(final$class%in%"1"); rr1[p] = 1
    gbo<-getbout(rr1,boutduration = boutduration*6,boutcriter = .9,ws3=win)
    e<-as.numeric(length(which(gbo$x == 1)))
    sedb<-round(e*win/60,2)
    e<-c(0,gbo$boutcount)
    e<-diff(e)
    sedbouts<-length(which(e==1))
    sedboutsavg<-sedb/sedbouts
    st<-which(e==1)
    end<-which(e==-1)
    if(length(st)>length(end)){end<-c(end,nrow(final))} #In case monitoring period ends with Sed bout
    if(length(end)>=1){end<-end-1}
    seddura<-end-st
    if(sedbouts>=1){
      dura = which(seddura == max(seddura))
      sedmaxdurast = st[dura]
      sedmaxduraend = end[dura]
      sedmaxdura<-round(length(which(gbo$x[sedmaxdurast:sedmaxduraend]==1))*win/60,2)
      sedmaxdurast<-final$time[sedmaxdurast]
      sedmaxduraend<-final$time[sedmaxduraend]
    }
    if(!sedbouts>=1){

      sedmaxdura<-NA
      sedmaxdurast<-NA
      sedmaxduraend<-NA
    }
    if(sedbouts>1){
      dura = which(seddura == min(seddura))
      sedmindurast = st[dura]
      sedminduraend = end[dura]
      sedmindura<-round(length(which(gbo$x[sedmindurast:sedminduraend]==1))*win/60,2)
      sedmindurast<-final$time[sedmindurast]
      sedminduraend<-final$time[sedminduraend]
    }
    if(sedbouts<=1){
      sedmindurast<-NA
      sedminduraend<-NA
      sedmindura<-NA
    }



    see[[i]]<-list(int.sed = int.sed,int.light = int.light,int.moderate = int.moderate,int.vigorous = int.vigorous,
                   class.sed = class.sed,class.stationaryplus = class.stationaryplus,class.walk = class.walk,class.run = class.run,
                   sleep = sleep, nw = nw, nw.mg = nw.mg, nw.mgvm = nw.mgvm, date = final$date[1], day = final$day[1],ID = final$subject[1],mvpa = mvpa, sedb = sedb,
                   start = st.sleep, end = end.sleep, start.day = st.day, end.day = end.day,
                   motionless = motionless, light.s = light.s, restless = restless, mvpabouts = mvpabouts, mvpamaxdura = mvpamaxdura,
                   mvpamaxdurast = mvpamaxdurast,mvpamaxduraend = mvpamaxduraend, mvpamindura = mvpamindura,
                   mvpamindurast = mvpamindurast, mvpaminduraend = mvpaminduraend, sedbouts = sedbouts, sedmaxdura = sedmaxdura, sedmaxdurast = sedmaxdurast,
                   sedmaxduraend = sedmaxduraend, sedmindura = sedmindura, sedmindurast = sedmindurast, sedminduraend = sedminduraend,
                   mvpaboutsavg = mvpaboutsavg, sedboutsavg = sedboutsavg, sleepperiod = sleepperiod)

  }

  z<-0

  for(i in 1:length(a)){
    z<-z+1
    aa<-see[[i]]
    mat[z,1]<-as.character(aa$ID);mat[z,2]<-as.character(aa$date);mat[z,3]<-as.character(aa$day);mat[z,4]<-as.character(aa$day);mat[z,5]<-aa$int.sed;mat[z,6]<-aa$int.light;mat[z,7]<-aa$int.moderate;mat[z,8]<-aa$int.vigorous
    mat[z,9]<-aa$class.sed;mat[z,10]<-aa$class.stationaryplus;mat[z,11]<-aa$class.walk;mat[z,12]<-aa$class.run;
    mat[z,13]<-aa$sleep;mat[z,14]<-aa$sleepperiod;mat[z,15]<-aa$nw
    mat[z,4]<-ifelse(mat[z,4]%in%c("Saturday","Sunday"),0,1)
    mat[z,16]<-aa$mvpa;mat[z,17]<-aa$mvpabouts;mat[z,18]<-aa$mvpaboutsavg;mat[z,19]<-aa$mvpamaxdura;mat[z,20]<-aa$mvpamaxdurast[1];mat[z,21]<-aa$mvpamaxduraend[1]
    mat[z,22]<-aa$mvpamindura;mat[z,23]<-aa$mvpamindurast[1];mat[z,24]<-aa$mvpaminduraend[1]
    mat[z,25]<-aa$sedb;mat[z,26]<-aa$sedbouts;mat[z,27]<-aa$sedboutsavg;mat[z,28]<-aa$sedmaxdura;mat[z,29]<-aa$sedmaxdurast[1];mat[z,30]<-aa$sedmaxduraend[1]
    mat[z,31]<-aa$sedmindura;mat[z,32]<-aa$sedmindurast[1];mat[z,33]<-aa$sedminduraend[1]
    mat[z,34]<-as.character(aa$start);mat[z,35]<-as.character(aa$start.day)
    if(length(aa$end)==0){aa$end<-NA}
    mat[z,36]<-as.character(aa$end)
    if(length(aa$end.day)==0){aa$end.day<-NA}
    mat[z,37]<-as.character(aa$end.day)
    mat[z,38]<-aa$motionless;mat[z,39]<-aa$light.s;mat[z,40]<-aa$restless
    mat[z,41]<-aa$nw.mg;mat[z,42]<-aa$nw.mgvm

  }



  return(list(mat = mat, allday = allday))
}
