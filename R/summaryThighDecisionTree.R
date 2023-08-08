#' Title
#'
#' @param ag
#' @param win
#'
#' @return
#' @export
#'
#' @examples
summary_thigh<-function(ag,win=10){
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
   day<-weekdays(as.Date(ag$date)) #this will be used when wanting to summarise data by day of week
  ag<-cbind(ag,day)
  allday<-ag
  ag<-ag[,c(1:3,4,7,19,20:24,28)]

  a<-unique(ag$date)
  mat<-matrix("",length(a),44)
  see<-list()

  for(i in 1:length(a)){
    final<-ag[ag$date%in%a[i],]
    intensity<-slide(final$intensity,width = (60/win),by=(60/win),Mode)
    class<-slide(final$class,width = (60/win),by=(60/win),Mode)
    nonwear<-slide(final$nonwear,width = (60/win),by=(60/win),Mode)
    sleep<-slide(final$sleep,width = (60/win),by=(60/win),Mode)
    time<-slide(final$time,width = (60/win),by=(60/win),function(x) head(x,n=1))

    int.sed<-round(as.numeric(length(which(intensity==1 & nonwear==0)))*60/60,2)
    int.light<-round(as.numeric(length(which(intensity==2 & nonwear==0)))*60/60,2)
    int.moderate<-round(as.numeric(length(which(intensity==3 & nonwear==0)))*60/60,2)
    int.vigorous<-round(as.numeric(length(which(intensity==4 & nonwear==0)))*60/60,2)

    class.sit<-round(as.numeric(length(which(class%in%"sit" & nonwear==0)))*60/60,2)
    class.lay<-round(as.numeric(length(which(class%in%"sit" & sleep%in%'s')))*60/60,2)
    class.standmov<-round(as.numeric(length(which((class%in%"move"|class%in%"stand") & nonwear==0)))*60/60,2)
    class.walk<-round(as.numeric(length(which(class%in%"walk" & nonwear==0)))*60/60,2)
    class.run<-round(as.numeric(length(which(class%in%"run" & nonwear==0)))*60/60,2)
    class.stairs<-round(as.numeric(length(which(class%in%"stairs" & nonwear==0)))*60/60,2)
    class.cycle<-round(as.numeric(length(which(class%in%"cycle" & nonwear==0)))*60/60,2)


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
      e<-as.numeric(length(which(final4$sleep%in%"sleep")))
      sleep<-round(e*win/60,2)
      e<-which(final4$sleep%in%"sleep")
      st.sleep<-final4$time[e[1]]
      end.sleep<-final4$time[e[length(e)]]
      st.day<-final4$date[e[1]]
      end.day<-final4$date[e[length(e)]]
      final4<-final4[e,]
      matt<-ifelse(final4$enmo*1000>=63.3,'r','l')
      post<-which(final4$bfsd.x>.013)
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
      st.sleep<-NA
      end.sleep<-NA
      st.day<-NA
      end.day<-NA
      motionless<-NA
      light.s<-NA
      restless<-NA
    }
    nw<-round(as.numeric(length(which(nonwear==1)))*60/60,2)
    nw.mg<-round(mean(final$enmo[which(final$nonwear==1)]*1000),5)
    nw.mgvm<-round(mean(final$bfvm[which(final$nonwear==1)]),5)

    boutduration = 10 * (60/60) #per 10 minutes

    rr1 = matrix(0,length(final$intensity),1)

    p = which(intensity==3|intensity==4); rr1[p] = 1
    gbo<-getbout(rr1,boutduration = boutduration,ws3=60)
    e<-as.numeric(length(which(gbo$x == 1)))
    mvpa<-round(e*60/60,2)
    e<-c(0,gbo$boutcount)
    e<-diff(e)
    mvpabouts<-length(which(e==1))
    mvpaboutsavg<-round(mvpa/mvpabouts,2)
    st<-which(e==1)
    end<-which(e==-1)
    if(length(st)>length(end)){end<-c(end,length(final))} #In case monitoring period ends with MVPA bout
    if(length(end)>=1){end<-end-1}
    mvpadura<-end-st
    if(mvpabouts>=1){
      dura = which(mvpadura == max(mvpadura))
      durast = st[dura]
      duraend = end[dura]
      mvpamaxdura<-round(length(which(gbo$x[durast:duraend]==1))*60/60,2)
      mvpamaxdurast<-time[durast]
      mvpamaxduraend<-time[duraend]
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
      mvpamindura<-round(length(which(gbo$x[mvpamindurast:mvpaminduraend]==1))*60/60,2)
      mvpamindurast<-time[mvpamindurast]
      mvpaminduraend<-time[mvpaminduraend]
    }
    if(mvpabouts<=1){
      mvpamindurast<-NA
      mvpaminduraend<-NA
      mvpamindura<-NA
    }

    rr1 = matrix(0,length(intensity),1)

    p = which(intensity==1 & nonwear == 0); rr1[p] = 1
    gbo<-getbout(rr1,boutduration = boutduration*6,boutcriter = .9,ws3=60)
    e<-as.numeric(length(which(gbo$x == 1)))
    sedb<-round(e*60/60,2)
    e<-c(0,gbo$boutcount)
    e<-diff(e)
    sedbouts<-length(which(e==1))
    sedboutsavg<-sedb/sedbouts
    st<-which(e==1)
    end<-which(e==-1)
    if(length(st)>length(end)){end<-c(end,length(intensity))} #In case monitoring period ends with Sed bout
    if(length(end)>=1){end<-end-1}
    seddura<-end-st
    if(sedbouts>=1){
      dura = which(seddura == max(seddura))
      sedmaxdurast = st[dura]
      sedmaxduraend = end[dura]
      sedmaxdura<-round(length(which(gbo$x[sedmaxdurast:sedmaxduraend]==1))*60/60,2)
      sedmaxdurast<-time[sedmaxdurast]
      sedmaxduraend<-time[sedmaxduraend]
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
      sedmindura<-round(length(which(gbo$x[sedmindurast:sedminduraend]==1))*60/60,2)
      sedmindurast<-time[sedmindurast]
      sedminduraend<-time[sedminduraend]
    }
    if(sedbouts<=1){
      sedmindurast<-NA
      sedminduraend<-NA
      sedmindura<-NA
    }



    see[[i]]<-list(int.sed = int.sed,int.light = int.light,int.moderate = int.moderate,int.vigorous = int.vigorous,
                   class.sit = class.sit,class.lay = class.lay,class.standmov = class.standmov,
                   class.walk = class.walk,class.run = class.run, class.stairs = class.stairs,class.cycle = class.cycle,
                   sleep = sleep, nw = nw, nw.mg = nw.mg, nw.mgvm = nw.mgvm, date = final$date[1], day = final$day[1],ID = final$subject[1],mvpa = mvpa, sedb = sedb,
                   start = st.sleep, end = end.sleep, start.day = st.day, end.day = end.day,
                   motionless = motionless, light.s = light.s, restless = restless, mvpabouts = mvpabouts, mvpamaxdura = mvpamaxdura,
                   mvpamaxdurast = mvpamaxdurast,mvpamaxduraend = mvpamaxduraend, mvpamindura = mvpamindura,
                   mvpamindurast = mvpamindurast, mvpaminduraend = mvpaminduraend, sedbouts = sedbouts, sedmaxdura = sedmaxdura, sedmaxdurast = sedmaxdurast,
                   sedmaxduraend = sedmaxduraend, sedmindura = sedmindura, sedmindurast = sedmindurast, sedminduraend = sedminduraend,
                   mvpaboutsavg = mvpaboutsavg, sedboutsavg = sedboutsavg)

  }


  mat[1,1]<-"ID";mat[1,2]<-"Date";mat[1,3]<-"Day";mat[1,4]<-"Weekend";mat[1,5]<-"int.Sed";mat[1,6]<-"int.Light";mat[1,7]<-"int.mod";mat[1,8]<-"int.vig"
  mat[1,9]<-"class.Sit";mat[1,10]<-"class.lay";mat[1,11]<-"class.standmov";mat[1,12]<-"class.walk";mat[1,13]<-"class.run"
  mat[1,14]<-"class.stairs";mat[1,15]<-"class.cycle"
  mat[1,16]<-"Sleep";mat[1,17]<-"NW"
  mat[1,18]<-"mvpa.b";mat[1,19]<-"mvpa.bouts";mat[1,20]<-'mvpa.b.avg';mat[1,21]<-'mvpa.b.max';mat[1,22]<-'mvpa.b.max.st';mat[1,23]<-'mvpa.b.max.end';mat[1,24]<-'mvpa.b.min';mat[1,25]<-'mvpa.b.min.st';mat[1,26]<-'mvpa.b.min.end'
  mat[1,27]<-"sed.b";mat[1,28]<-'sed.bouts';mat[1,29]<-'sed.b.avg';mat[1,30]<-'sed.b.max';mat[1,31]<-'sed.b.max.st';mat[1,32]<-'sed.b.max.end';mat[1,33]<-'sed.b.min';mat[1,34]<-'sed.b.min.st';mat[1,35]<-'sed.b.min.end'
  mat[1,36]<-"sleep.start";mat[1,37]<-"sleep.end";mat[1,38]<-"st.day";mat[1,39]<-"end.day"
  mat[1,40]<-"motionless";mat[1,41]<-"light.s";mat[1,42]<-"restless"
  mat[1,43]<-"nw.mg";mat[1,44]<-"nw.mgvm"
  z<-0
  for(i in 1:length(a)){
    z<-z+1
    aa<-see[[i]]
    mat[z,1]<-as.character(aa$ID);mat[z,2]<-as.character(aa$date);mat[z,3]<-as.character(aa$day);mat[z,4]<-as.character(aa$day);mat[z,5]<-aa$int.sed;mat[z,6]<-aa$int.light;mat[z,7]<-aa$int.moderate;mat[z,8]<-aa$int.vigorous
    mat[z,9]<-aa$class.sit;mat[z,10]<-aa$class.lay;mat[z,11]<-aa$class.standmov;mat[z,12]<-aa$class.walk;mat[z,13]<-aa$class.run
    mat[z,14]<-aa$class.stairs;mat[z,15]<-aa$class.cycle
    mat[z,16]<-aa$sleep;mat[z,17]<-aa$nw
    mat[z,4]<-ifelse(mat[z,4]%in%c("Saturday","Sunday"),0,1)
    mat[z,18]<-aa$mvpa;mat[z,19]<-aa$mvpabouts;mat[z,20]<-aa$mvpaboutsavg;mat[z,21]<-aa$mvpamaxdura;mat[z,22]<-aa$mvpamaxdurast[1];mat[z,23]<-aa$mvpamaxduraend[1];mat[z,24]<-aa$mvpamindura;mat[z,25]<-aa$mvpamindurast[1];mat[z,26]<-aa$mvpaminduraend[1]
    mat[z,27]<-aa$sedb;mat[z,28]<-aa$sedbouts;mat[z,29]<-aa$sedboutsavg;mat[z,30]<-aa$sedmaxdura;mat[z,31]<-aa$sedmaxdurast[1];mat[z,32]<-aa$sedmaxduraend[1];mat[z,33]<-aa$sedmindura;mat[z,34]<-aa$sedmindurast[1];mat[z,35]<-aa$sedminduraend[1]
    mat[z,36]<-as.character(aa$start);mat[z,37]<-as.character(aa$start.day)
    if(length(aa$end)==0){aa$end<-NA}
    mat[z,38]<-as.character(aa$end)
    if(length(aa$end.day)==0){aa$end.day<-NA}
    mat[z,39]<-as.character(aa$end.day)
    mat[z,40]<-aa$motionless;mat[z,41]<-aa$light.s;mat[z,42]<-aa$restless
    mat[z,43]<-aa$nw.mg;mat[z,44]<-aa$nw.mgvm

  }



  return(list(mat = mat, allday = allday))
}

