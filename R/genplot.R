#' Title
#'
#' @param final
#' @param window
#' @param output
#' @param folder_name
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
GenPlot_general<-function(final,window=win,output,folder_name=folder_name,file_name=file_name)
{

    a<-unique(final$date)
    ID<-final$subject[1]
    cat(paste("\n\nGenerating Daily Visual Plots\n",sep = " "))

    mypath<-file.path(output,paste("/scored files",folder_name,"/daily plots/",ID,file_name,"_activity plot",".pdf",sep = ""))
    pdf(file=mypath,width=20,height = 10)
    #pdf(file=mypath,width = 1100,height = 800)
    #par(mfrow=c(3,3))

    for(i in 1:length(a)){
      par(mfrow=c(2,1))
      cat(paste("processing day",i,"out of",length(a),"\r",sep = " "))
      see2<-final[final$date==a[i],]
      time2<- strptime(see2$time,"%H:%M:%S")
      time3<- format(time2, "%H:%M:%S")
      zz<-which(time3>="12:00:00")
      zz2<-which(time3<"12:00:00")

      try(for(zzz in 1:2){
        if(zzz==1){see<-see2[zz2,]}
        if(zzz==2){see<-see2[zz,]}
        dat<-weekdays(as.Date(see$date[1]))


        ##################
        ####################
        #######################
        if(dim(see)[1]==0){plot(NA,ylim=range(1,2),xlim=range(1,2),ylab="",xlab="",main="",axes=F)

          text(y = 1.5, 1.8, labels = "NO DATA FOR THIS TIME PERIOD", srt = 0, pos = 2, xpd = TRUE,cex=3)}

        if(dim(see)[1]>0)
        {
          plot(see$enmo*1000, main=paste(dat,a[i]," ",see$time[1],"-",see$time[dim(see)[1]],sep = " "),ylim=range(-1200,1800),axes=F,type="l",xlab= "time",ylab = "ENMO",cex.lab=1.5,cex.axis=1.5) #"ylim" should be made to the min and max values of the feature being plotted


          axis(1,at=c(1,nrow(see)*.25,nrow(see)*.5,nrow(see)*.75,nrow(see)),
               c(see$time[1],see$time[length(see$time)*.25+1],see$time[length(see$time)*.5+1],
                 see$time[length(see$time)*.75+1],see$time[nrow(see)])) #puts time stamps on x axis

          axis(2,at=seq(0,1800,600),seq(0,1800,600)) #this leaves enought room at bottom of y-axis for rug plots, hence the low of -5
          lines(see$tilt+500)
        }
        ##########
        ##########
        d<-rep(NA,dim(see)[1]) #this creates a vector of NA that is equal to length of time-series
        e<-which(see$class==1) #this extracts only indices that are activity=1
        d[e]<-"gray34" #this makes all values where an index is activity 1 as "black" and the rest stay as NA

        for (iii in 1:dim(see)[1])
        {

          rug(iii,side=1,col=d[iii],line=0,lwd=.001)

        }
        ##############
        ###############
        d<-rep(NA,dim(see)[1]) #repeat process for activity=2
        e<-which(see$class==2)
        d[e]<-"blue"

        for (iii in 1:dim(see)[1])
        {

          rug(iii,side=1,col=d[iii],line=-.85,lwd=.001) #for a rug-plot "line" value, "-1" will be written above line=0

        }
        ##############
        ###############
        d<-rep(NA,dim(see)[1])
        e<-which(see$class==3)
        d[e]<-"orange"

        for (iii in 1:dim(see)[1])
        {

          rug(iii,side=1,col=d[iii],line=-1.90,lwd=.001)

        }
        ##############
        ###############
        d<-rep(NA,dim(see)[1])
        e<-which(see$class==4)
        d[e]<-"purple"

        for (iii in 1:dim(see)[1])
        {

          rug(iii,side=1,col=d[iii],line=-2.90,lwd=.001)

        }

        d<-rep(NA,dim(see)[1])
        e<-which(see$class==5)
        d[e]<-"red"

        for (iii in 1:dim(see)[1])
        {

          rug(iii,side=1,col=d[iii],line=-4,lwd=.001)

        }
        ##############
        ###############
        d<-rep(NA,dim(see)[1])
        e<-which(see$class==6)
        d[e]<-"skyblue"

        for (iii in 1:dim(see)[1])
        {

          rug(iii,side=1,col=d[iii],line=-5,lwd=.001)

        }
        ##############
        ###############
        d<-rep(NA,dim(see)[1])
        e<-which(see$class==7)
        d[e]<-"black"

        for (iii in 1:dim(see)[1])
        {

          rug(iii,side=1,col=d[iii],line=-6,lwd=.001)

        }

        labels <- c(-1280,-1100,-900,-700,-500,-310,-150) #this indicates where on the Y axis to put class labels, and will need to be adjusted depending on how many classes there are in a model

        text(y = labels,x=1, par("usr")[1]-.07, labels = c("SED","LIGHT","MOD","Walk","Run","Sleep","NW"), srt = 0, pos = 2, xpd = TRUE,cex=.60) #puts class labels next to rug plot
        ##############
        ###############PLOT MVPA BOUTS
        boutduration = 10 * (60/window) #per 10 minutes

        rr1 = matrix(0,length(see$enmo),1)

        p = which(see$class==3|see$class==4|see$class==5); rr1[p] = 1
        gbo<-getbout(rr1,boutduration = boutduration,ws3=window)
        d<-rep(NA,dim(see)[1])
        e<-which(gbo$x==1)
        d[e]<-"skyblue"

        for (iii in 1:dim(see)[1])
        {

          rug(iii,side=3,col=d[iii],line=0,lwd=.001)

        }
        ##############
        ###############PLOT SED BOUTS
        boutduration = 60 * (60/window) #per 60 minutes

        rr1 = matrix(0,length(see$enmo),1)

        p = which(see$class==1); rr1[p] = 1
        gbo<-getbout(rr1,boutduration = boutduration,boutcriter = .9,ws3=window)
        d<-rep(NA,dim(see)[1])
        e<-which(gbo$x==1)
        d[e]<-"darkgreen"

        for (iii in 1:dim(see)[1])
        {

          rug(iii,side=3,col=d[iii],line=-3,lwd=.001)

        }
        labels <- c(1900,1200) #this indicates where on the Y axis to put class labels, and will need to be adjusted depending on how many classes there are in a model

        text(y = labels,x=1, par("usr")[1], labels = c("MV.B","SED.B"), srt = 0, pos = 2, xpd = TRUE,cex=.5) #puts class labels next to rug plot
        ##############

      },silent = T)

      if(i==length(a)){
        for(ss in 1:length(a)){
          if(ss==1|ss==9|ss==17|ss==25){par(mfrow=c(4,2))}

          final3<-final[final$date==a[ss],]
          dat<-weekdays(as.Date(final3$date[1]))
          plot(NULL,xlim=range(-5,5),ylim=range(-10,10),axes=F,ylab="",xlab="",
               main = paste( dat, final3$date[1],sep = " "),cex.main=2)
          box()
          rp = vector('expression',5) #These next few lines will sum up time spent in min for each class and put on plot
          e<-as.numeric(length(which(final3$class==1)))
          e<-round(e*window/60,2)
          rp[5]<-paste("Time spent in SED activities:",e, "mins" ,sep = " ")
          e<-as.numeric(length(which(final3$class==2)))
          e<-round(e*window/60,2)
          rp[4]<-paste("Time spent in pottering/light activities:",e, "mins" ,sep = " ")
          e<-as.numeric(length(which(final3$class==3)))
          e<-round(e*window/60,2)
          rp[3]<-paste("Time spent in active game play:",e, "mins" ,sep = " ")
          e<-as.numeric(length(which(final3$class==4)))
          e<-round(e*window/60,2)
          rp[2]<-paste("Time spent walking:",e, "mins" ,sep = " ")
          e<-as.numeric(length(which(final3$class==5)))
          e<-round(e*window/60,2)
          rp[1]<-paste("Time spent running:",e, "mins" ,sep = " ")
          legend("topleft", bty="n", legend=rp,cex=1.5)



          rp = vector('expression',4)
          sss=ss+1
          if(sss<=length(a)){
            final4<-final[final$date==a[ss]|final$date==a[sss] ,]
            time2<- strptime(paste(final4$date, final4$time,sep = ""),"%Y-%m-%d %H:%M:%S")
            final4$time3<- format(time2, "%H:%M:%S")
            z<-which(final4$date==a[ss] & final4$time3>="12:00:00")
            z<-z[1]
            x<-which(final4$date==a[sss] & final4$time3<="12:00:00")
            x<-x[length(x)]
            x<-x-1
            final4<-final4[z:x,]
            e<-as.numeric(length(which(final4$intensity==6)))
            e<-round(e*window/60,2)
            rp[4]<-paste("Time spent Sleeping:",e, "mins" ,sep = " ")
          }
          if(sss>length(a)){rp[4]<-"No Sleep On Last Day"}
          e<-as.numeric(length(which(final3$intensity==7)))
          e<-round(e*window/60,2)
          rp[3]<-paste("Nonwear:",e, "mins" ,sep = " ")

          boutduration = 10 * (60/window) #per 10 minutes
          rr1 = matrix(0,length(final3$enmo),1)

          p = which(final3$class==3|final3$class==4|final3$class==5); rr1[p] = 1
          gbo<-getbout(rr1,boutduration = boutduration,ws3=window)
          e<-as.numeric(length(which(gbo$x == 1)))
          e<-round(e*window/60,2)
          rp[2]<-paste("MVPA bout:",e, "mins" ,sep = " ")

          boutduration = 60 * (60/window) #per 60 minutes
          rr1 = matrix(0,length(final3$enmo),1)

          p = which(final3$class==1); rr1[p] = 1
          gbo<-getbout(rr1,boutduration = boutduration,boutcriter = .9,ws3=window)
          e<-as.numeric(length(which(gbo$x == 1)))
          e<-round(e*window/60,2)
          rp[1]<-paste("SED bout:",e, "mins" ,sep = " ")
          legend("topright", bty="n", legend=rp,cex=1.5)
        }}
    }


    invisible(dev.off())

}

