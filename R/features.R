#' Title
#'
#' @param output
#' @param win
#' @param axis
#' @param Sed
#' @param Mod
#' @param Vig
#' @param Calibrate
#' @param visual
#' @param Guide
#' @param sleep
#' @param dothis
#' @param ndays
#' @param folder_name
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
features<-function(input,output,win=15,axis="yaxis",Sed=200,Mod=420,Vig=842,Calibrate=F,visual=T,Guide=T,sleep=T,
                   dothis=dothis,ndays=12,folder_name="",file_name="")

{
  #To do list: ndays utility, update Link to match gt3x read in, only call in functions when needed
  # custom folder names, sleep/wake, lag/lead models, timestamp, remove unused models
  list.of.packages <- c("plyr", "dplyr","e1071","zoo","data.table","randomForest",
                        "readxl","Rcpp","HMM","rattle","GENEAread","signal","GGIR","GGIRread","TLBC","DescTools","tools")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  # if(length(new.packages)){
  #   print(paste("Installing dependancy package:",new.packages))
  #   install.packages(new.packages)
  # }
  library(plyr)
  library(dplyr)
  library(e1071)
  library(data.table)
  library(randomForest)
  library(readxl)
  library(Rcpp)
  library(HMM)
  library(rattle)
  library(GENEAread)
  library(signal)
  library(GGIR)
  library(GGIRread)
  library(TLBC)
  library(DescTools)
  library(tools)
  options(digits.secs=3)
  oldw <- getOption("warn")
  options(warn = -1)
  #  # source(paste(mypath,"/feature extraction.R",sep = ""))
  #  # source(paste(mypath,"/freq domain.R",sep = ""))
  #  # source(paste(mypath,"/lag.R",sep = ""))
  #  # source(paste(mypath,"/cross corr.R",sep=""))
  #  # source(paste(mypath,"/read_accel.R",sep = ""))
  #  # source(paste(mypath,"/round_df.R",sep = ""))
  #  # source(paste(mypath,"/slide.R",sep = ""))
  #  # source(paste(mypath,"/time domain.R",sep = ""))
  #  # source(paste(mypath,"/summaryV2.R",sep = ""))
  #  # source(paste(mypath,"/nonwear_1min.R",sep = ""))
  #  # source(paste(mypath,"/read_excel.R",sep = ""))
  #  # source(paste(mypath,"/read_counts.R",sep = ""))
  #  # source(paste(mypath,"/sleep detection.R",sep = ""))
  #  # source(paste(mypath,"/detect_sleep_periods.R",sep = ""))
  #  # source(paste(mypath,"/IntensityCutPoint.R",sep = ""))
  #  # source(paste(mypath,"/getbout.R",sep = ""))
  #  # source(paste(mypath,"/note.R",sep = ""))
  #  # source(paste(mypath,"/choice.R",sep = ""))
  #  # source(paste(mypath,"/genplot.R",sep = ""))
  #  # source(paste(mypath,"/read_gt3x.R",sep = ""))
  #  # source(paste(mypath,"/parse_txt.R",sep = ""))
  #  # source(paste(mypath,"/tick.R",sep = ""))
  #  # source(paste(mypath,"/read_acceleration.R",sep = ""))
  #  # source(paste(mypath,"/test2.R",sep = ""))
  #  # source(paste(mypath,"/partialdaysummary.R",sep = ""))
  #  # source(paste(mypath,"/partialdaysummary.R",sep = ""))
  #  # source(paste(mypath,"/mattcalibrate.R",sep = ""))
  #  # source(paste(mypath,"/EllipsoidFit.R",sep = ""))
  #  # source(paste(mypath,"/center_radius.R",sep = ""))
  #  # source(paste(mypath,"/read_accelerationLink.R",sep = ""))
  #  # source(paste(mypath,"/readGA.R",sep = ""))
  #  # source(paste(mypath,"/readax3.R",sep = ""))
  #  # source(paste(mypath,"/cwa.R",sep = ""))
  #  # source(paste(mypath,"/GN function_20191024.R",sep = ""))




  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  #Designate activity, intensity, sleep, calibration, etc. parameters----
  #moo<-choice()
  moo<-dothis
  Classifier<-moo[1]
  Intensity<-moo[2]
  Intensity_Type<-moo[3]
  Sed<-as.numeric(moo[4])
  Mod<-as.numeric(moo[5])
  Vig<-as.numeric(moo[6])
  axis<-moo[7]
  en<-as.numeric(moo[8])
  note(Classifier,Mod)
  CalibrateMethod<-"SphereFit"
  #Create output folder----
  ifelse(!dir.exists(file.path(paste(output, "/scored files",folder_name,sep=""))),
         dir.create(file.path(paste(output, "/scored files",folder_name,sep=""))), FALSE) #creates a file directory
  #Identify files inside input folder----
  AG.file <-
    list.dirs(input,recursive=T)
  AG.file<-AG.file[which(AG.file%in%input)]

  if(Guide==TRUE){
    see<-read_excel_allsheets(excel)
    part<-see[[1]]
  }
  else{
    part<-matrix()
    #part$ID<-Sys.glob(paste(AG.file,"/*.gt3x",sep = ""))
    part$ID <- list.files(AG.file,recursive = F,full.names = T)
    part$ID <- part$ID[grep(".csv|.gt3x|.bin|.cwa", part$ID, fixed=F)]
    ct<-which(part$ID%in% part$ID[grep("sec.csv", part$ID, fixed=F)])
    if(length(ct)>0){ part$ID<-part$ID[-ct] }
    #if(!length(part$ID)>=1){part<-matrix();part$ID<-Sys.glob(paste(AG.file,"/*.csv",sep = ""));bindata=F }
  }

  cat(paste("\n\n-------",length(part$ID),"accelerometer files will be processed and scored","-------","\n\n",sep=" "))
  for (i in 1:length(part$ID))
  {

    if(Guide==TRUE){
      #AG<-Sys.glob(paste(AG.file,"/*",part$ID[i],"*.gt3x",sep = ""))
      AG<-Sys.glob(paste(AG.file,"/*",part$ID[i],sep = ""))

    }
    else{
      AG<-part$ID[i]
    }
    bindata<-tools::file_ext(AG)
    ID<-basename(AG)
    ID<-tools::file_path_sans_ext(ID)

    #Read in monitors ----
    cat(paste("\nProcessing:",ID,"\n",sep=" "))
    if(Guide==TRUE){
      # dat<-paste(part$`Start Date`[i],part$Start[i]) May remove if not used anymore
      # dat<-unlist(strsplit(dat,split=' ',fixed=TRUE))
      # dat<-paste(dat[1],dat[3])
      # start <- strptime(dat,format="%Y-%m-%d %H:%M:%S")
      # dat<-paste(part$`End Date`[i],part$End[i])
      # dat<-unlist(strsplit(dat,split=' ',fixed=TRUE))
      # dat<-paste(dat[1],dat[3])
      # stop <- strptime(dat,format="%Y-%m-%d %H:%M:%S")
    }

    if(bindata%in%"gt3x"){
      cat(paste("\nReading In .GT3X Accelerometer Data","\n",sep=" "))
      cat(paste("File Size:",utils:::format.object_size(file.info(AG)$size, "auto")))
      hip.data <- read_gt3x(AG)
      cat(paste("\nDevice Serial Number:",  hip.data$header$Serial_Number,'\n'))
      cat(paste("Firmware:", hip.data$header$Firmware,'\n'))
      cat(paste("Sample Frequency:",hip.data$header$Sample_Rate,'\n'))
      cat(paste("Acceleration Scale:",hip.data$header$Acceleration_Scale,'\n'))
      cat(paste("Min g's:",hip.data$header$Acceleration_Min, "/", "Max g's:",hip.data$header$Acceleration_Max,'\n'))
      cat(paste("Download Time:", hip.data$header$Last_Sample_Time,'\n'))
      cat(paste("Finished Reading In .GT3X Accelerometer Data","\n",sep=" "))
    }
    if(bindata%in%"csv"){
      hip.data<-read.accel(AG)
      if(hip.data$mon==1){
        colnames(hip.data$data)<-c("x","y","z","temp") #Geneactiv
      }
      if(hip.data$mon==2){
        colnames(hip.data$data)<-c("x","y","z") #ActiGraph
      }
      if(hip.data$mon==3){
        colnames(hip.data$data)<-c("x","y","z") #ActivPal
      }
      hip.data$header<-data.frame(1,1)
      hip.data$header$Sample_Rate<-hip.data$Fs
    }
    if(bindata%in%"bin"){
      hip.data<-readGA(AG)
      hip.data$header<-data.frame(1,1)
      zz<-GENEAread::header.info(AG)
      sf<-as.numeric(unlist(zz$Value[2]))
      hip.data$header$Sample_Rate<-sf


    }
    if(bindata%in%"cwa"){
      hip.data<-readax3(AG) #2020/19/11 AX3 has temp, battery, and light data. These have been removed
      #to save memory space and speed up reading in data. Can be included in the future if needed
      hip.data$header<-data.frame(1,1)
      hip.data$header$Sample_Rate<-hip.data$Fs
    }
    #Calibration start----
    if(Calibrate==T & CalibrateMethod%in%"SphereFit"){
      ag<-try(calibrateGN(hip.data$data,Fs=hip.data$header$Sample_Rate),silent=T)
      # if(class(ag)== "try-error"){
      # ag<-try(calibrate(hip.data$data,Fs=hip.data$header$Sample_Rate),silent = T)
      # }
      if(!class(ag)=="try-error"){
        if(ag$vm.error.end<ag$vm.error.st & ag$vm.error.end<50)
        {
          start = increment = constant = (hip.data$header$Sample_Rate*60*60*24) #variables used to read data in 24 hr increment
          LD<-0
          LD2<-0
          count<-1
          chunk<-1
          chunk2<-dim(hip.data$data)[1]
          while(LD<1){


            if(increment>=dim(hip.data$data)[1]){
              increment<-dim(hip.data$data)[1] #if data is less than 24 hrs, set to end of data
              LD2<-1
            }
            cat(paste("\rCalibrating hours",round((1+(start *(chunk -1)))/hip.data$header$Sample_Rate/3600,2),"to",round(increment/hip.data$header$Sample_Rate/3600,2),
                      "out of",round(chunk2/hip.data$header$Sample_Rate/3600,2),"\r", sep = " "))

            hip.data$data[(1+(start *(chunk -1))):(increment),1]<-
              ag$scale[1]*(hip.data$data[(1+(start *(chunk -1))):(increment),1]-ag$offset[1])
            hip.data$data[(1+(start *(chunk -1))):(increment),2]<-
              ag$scale[2]*(hip.data$data[(1+(start *(chunk -1))):(increment),2]-ag$offset[2])
            hip.data$data[(1+(start *(chunk -1))):(increment),3]<-
              ag$scale[3]*(hip.data$data[(1+(start *(chunk -1))):(increment),3]-ag$offset[3])

            count = count + (increment)
            increment = increment+constant
            chunk=chunk+1
            if(LD2==1){
              LD<-1
              e<-which(is.na(hip.data$data[,1]))
              if(length(e)>0){
                hip.data$data<-hip.data$data[-e,]
              }
            }
          }
        }
        else{
          cat("Calibration Results Not Implemented Because Values Do Not Decrease Error")
        }
      }
      else{
        cat("Device Was Not Calibrated. Not Enough Orientation Changes")
      }
    }

    if(Calibrate==F){
      ag<-"calibration = False"
    }
    #Calibration end----
    cat("\n")
    # Preschool RF Hip/ Wrist/ Lag-Lead and LVAY wrist RF----
    if(Classifier%in%"Preschool Wrist Random Forest Free Living"|Classifier%in%"School age Wrist Random Forest"|Classifier%in%"Preschool Hip Random Forest Free Living"|Classifier%in%"Preschool Wrist Random Forest Free Living Lag-Lead"|Classifier%in%"Preschool Hip Random Forest Free Living Lag-Lead"){
      if(i==1){
        win=15
        #  # source(paste(mypath,"/RFClassifier.R",sep = ""))
        if(Classifier%in%"Preschool Hip Random Forest Free Living Lag-Lead"){
          # load(paste(mypath,"/PS.RF.FL.Hip15.LagLead.RData",sep=""))
          rfmodel<-PS.RF.FL.Hip15.LagLead
          # rm(PS.RF.FL.Hip15.LagLead)
        }
        if(Classifier%in%"Preschool Wrist Random Forest Free Living Lag-Lead"){
          # load(paste(mypath,"/PS.RF.FL.Wrist15.LagLead.RData",sep=""))
          rfmodel<-PS.RF.FL.Wrist15.LagLead
          # rm(PS.RF.FL.Wrist15.LagLead)
        }
        if(Classifier%in%"Preschool Wrist Random Forest Free Living"){
          # load(paste(mypath,"/PS.RF.FL.Wrist15.Rdata",sep=""))
          rfmodel<-preschooltest
          # rm(preschooltest)
        }

        if(Classifier%in%"School age Wrist Random Forest"){
          # load(paste(mypath,"/LVAY.RF.Wrist5.1.Rdata",sep=""))
          rfmodel<-LVAY.RF.Wrist5.1
          # rm(LVAY.RF.Wrist5.1)
        }
        if(Classifier%in%"Preschool Hip Random Forest Free Living"){
          # load(paste(mypath,"/PS.RF.FL.Hip15.RData",sep=""))
          rfmodel<-preschool_hipfl_15s
          # rm(preschool_hipfl_15s)
        }
      }
      classifier<-classifier_Preschool(raw=hip.data$data,Fs=hip.data$header$Sample_Rate,ID=ID,
                                     mypath=mypath,sleep=sleep,win=win,rfmodel=rfmodel,Classifier=Classifier,start.time=hip.data$dat)}
    # LVAY Hip RF (2019/10/04) ----
    if(Classifier%in%"School age Hip Random Forest"){
      if(i==1){
        win=10
         # source(paste(mypath,"/RFClassifier.R",sep = ""))
        # load(paste(mypath,"/LVAY.RF.Hip10.RData",sep=""))
        rfmodel<-LVAY.RF.Hip10
        # rm(LVAY.RF.Hip10)
      }
      classifier<-classifier_Preschool(raw=hip.data$data,Fs=hip.data$header$Sample_Rate,ID=ID,
                                     mypath=mypath,sleep=sleep,win=win,rfmodel=rfmodel,Classifier=Classifier,start.time=hip.data$dat)
    }

    # Ellis RF/HMM Wrist (12/04/2019); Ellis HMM/RF (Hip 09/07/2019)----
    if(Classifier%in%"Ellis Wrist RF"|Classifier%in%"Ellis Hip RF"){
      if(i==1){
         # source(paste(mypath,"/EllisClassifier.R",sep = ""))
         # source(paste(mypath,"/Ellis.feat.extraction.R",sep = ""))
        win=60
      }
      classifier<-classifier_Ellis(raw=hip.data$data,Fs=hip.data$header$Sample_Rate,ID=ID,
                                     mypath=mypath,win=win,Classifier=Classifier,sleep=sleep,start.time=hip.data$dat)}

    #Trost Adult RF Wrist (19/06/2019) ----
    if(Classifier%in%"Trost Adult Wrist RF"){
      if(i==1){
        #  # source(paste(mypath,"/TrostAdultRFClassifier.R",sep = ""))
        win=10
        if(Classifier%in%"Trost Adult Wrist RF"){
          # load(paste(mypath,"/trostRF_7112014.RData",sep=""))
          rfmodel<-trostRF_7112014
          # rm(trostRF_7112014)
        }
      }
      classifier<-classifier_TrostAdult(raw=hip.data$data,Fs=hip.data$header$Sample_Rate,ID=ID,
                                     mypath=mypath,win=win,sleep=sleep,rfmodel=rfmodel,Classifier=Classifier,start.time=hip.data$dat)}
    #Thigh Decision Tree; testing, not for use (09/10/2020)----
    if(Classifier%in%"Thigh Decision Tree"){
      if(i==1){
        #  # source(paste(mypath,"/ThighClassifier.R",sep = ""))
        win=10
      }
      classifier<-classifier_thigh(raw=hip.data$data,Fs=hip.data$header$Sample_Rate,ID=ID,
                                     mypath=mypath,win=win,sleep=sleep,Classifier=Classifier,start.time=hip.data$dat)}
    # Enmo only (23/07/2019)----
    if(Classifier%in%"Only do enmo"){
      if(i==1){
        win=en
        #  # source(paste(mypath,"/EnmoClassifier.R",sep = ""))
      }
      classifier<-EnmoClassifier(raw=hip.data$data,Fs=hip.data$header$Sample_Rate,ID=ID,
                                 mypath=mypath,win=win,sleep=sleep,start.time=hip.data$dat)}

    #If Guide document is used----
    if(Guide==TRUE){
      ts<-strptime(paste(classifier$Activity$date,classifier$Activity$time),format="%Y-%m-%d %H:%M:%S")
      classifier$Activity<-classifier$Activity[ts>=start & ts<=stop,]
    }


    classifier<-list(Activity= classifier$Activity,FS = hip.data$header$Sample_Rate)
    cat("\nCompleted Activity Classification\n")
    ifelse(!dir.exists(file.path(paste(output, "/scored files",folder_name,"/raw",sep=""))),
           dir.create(file.path(paste(output, "/scored files",folder_name,"/raw",sep=""))), FALSE)

    save(classifier, file=paste(output,"/scored files",folder_name,"/raw","/",ID,"_raw.scored",".RData",sep=""))
    #If activity counts are used----
    if(Intensity==T & Intensity_Type%in%"Counts"){
      cat("\nReading In Count Data\n")
      AG.count<-Sys.glob(paste(AG.file,"/*",ID,"*sec.csv",sep = ""))
      count<-read.counts(AG.count)
      if(count$epoch!=win){
        if(count$epoch>win){
          stop(cat("epoch value is greater than classifier window. Can't Proceed"))
        }
        if(!is.wholenumber(win/count$epoch)){
          stop(cat("epoch can not be scaled to classifier window "))
        }}

      if(is.wholenumber(win/count$epoch)){
        time<-slide(count$data[,1],width=win/count$epoch ,FUN= function(x) head(x,n=1))#################### FIX WINDOW SIZE AND OVERLAP ACCORDINGLY
        class(time) = c('POSIXt','POSIXct')
        Axis1<-slide(count$data[,3],width=win/count$epoch ,FUN= sum)
        Axis2<-slide(count$data[,2],width=win/count$epoch ,FUN= sum)
        Axis3<-slide(count$data[,4],width=win/count$epoch ,FUN= sum)
        count$data<-data.frame(time,Axis1,Axis2,Axis3)
      }

      colnames(count$data)<-c("time","Axis1","Axis2","Axis3")
      if(Guide==TRUE){
        count<-count$data[count$data$time>=start & count$data$time<=stop, ]
      }
      else{
        count<-count$data
      }
      if(length(Mod)>0){
        intensity<-IntensityCounts(count,axis=axis,Sed=Sed,Mod=Mod,Vig=Vig)}

      ifelse(!dir.exists(file.path(paste(output, "/scored files",folder_name,"/counts",sep=""))),
             dir.create(file.path(paste(output, "/scored files",folder_name,"/counts",sep=""))), FALSE)

      save(intensity, file=paste(output,"/scored files",folder_name,"/counts","/",ID,"_counts.scored",".RData",sep=""))
    }
    #If raw data is used----
    if(Intensity==T & Intensity_Type%in%"Raw"){
      cat("\nReading In Raw Intensity Data\n")
      indexes <- which(colnames(classifier$Activity)%in%c("enmo"))
      count<-data.frame(classifier$Activity[,c(indexes,4,5,6)])

      if(length(Mod)>0){
        intensity<-IntensityCounts(count,axis=axis,Sed=Sed,Mod=Mod,Vig=Vig)}

      ifelse(!dir.exists(file.path(paste(output, "/scored files",folder_name,"/counts",sep=""))),
             dir.create(file.path(paste(output, "/scored files",folder_name,"/counts",sep=""))), FALSE)

      save(intensity, file=paste(output,"/scored files",folder_name,"/counts","/",ID,"_counts.scored",".RData",sep=""))
    }
    #Generate summary report single file----
    cat("\nCompleting Summary Report\n")

    load(paste(output,"/scored files",folder_name,"/raw","/",ID,"_raw.scored",".RData",sep=""))
    load(paste(output,"/scored files",folder_name,"/counts","/",ID,"_counts.scored",".RData",sep=""))

    if(nrow(intensity$count)>nrow(classifier$Activity)){intensity$count<-intensity$count[1:nrow(classifier$Activity),]}
    if(nrow(classifier$Activity)>nrow(intensity$count)){classifier$Activity<-classifier$Activity[1:nrow(intensity$count),]}
    if(!Classifier%in%"Only do enmo"){
      intensity$count$intensity[which(classifier$Activity$class==6)]<-6
      intensity$count$intensity[which(classifier$Activity$class==7)]<-7
      intensity$count$intensity[which(classifier$Activity$class==8)]<-8
      comb<-cbind(classifier$Activity,intensity=intensity$count[,5], xaxis=intensity$count[,2],
                  yaxis=intensity$count[,3],zaxis=intensity$count[,4])
    }else{
      intensity$count$intensity_comb<-intensity$count$intensity
      intensity$count$intensity_comb[which(classifier$Activity$class==6)]<-6
      intensity$count$intensity_comb[which(classifier$Activity$class==7)]<-7
      intensity$count$intensity_comb[which(classifier$Activity$class==8)]<-8
      comb<-cbind(classifier$Activity,intensity=intensity$count[,5],intensity_comb=intensity$count[,6])
    }


    if(Classifier%in%"Preschool Wrist Random Forest Free Living Lag-Lead"|Classifier%in%"Preschool Hip Random Forest Free Living Lag-Lead"|Classifier%in%"Preschool Wrist Random Forest Free Living"|Classifier%in%"School age Wrist Random Forest"|Classifier%in%"School age Hip Random Forest"|Classifier%in%"Preschool Hip Random Forest Free Living"){
      comb<-summary_general(comb,win=win)
      colnames(comb$mat)<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
                            "class.Sed","class.Light","class.MV","class.Walk","class.Run",
                            "Sleep","Sleep.Period","Non-wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max','mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
                            "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
                            "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
                            "nw.mg","nw.mgvm")
    }
    if(Classifier%in%"Ellis Wrist RF"|Classifier%in%"Ellis Hip RF"){
      #  # source(paste(mypath,"/summaryellis.R",sep = ""))
      comb<-summary_Ellis(comb,win=win)
      colnames(comb$mat)<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
                            "class.biking","class.sed","class.standmov","class.standstill","class.walk",
                            "Sleep","Sleep.Period","Non-wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max','mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
                            "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
                            "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
                            "nw.mg","nw.mgvm","class.vehicle")
    }
    if(Classifier%in%"Trost Adult Wrist RF"){
      #  # source(paste(mypath,"/summarytrostadultrf.R",sep = ""))
      comb<-summary_TrostAdult(comb,win=win)
      colnames(comb$mat)<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
                            "class.sed","class.stationaryplus","class.walk","class.run",
                            "Sleep","Sleep.Period","Non-wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max','mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
                            "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
                            "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
                            "nw.mg","nw.mgvm")
    }
    if(Classifier%in%"Thigh Decision Tree"){
      #  # source(paste(mypath,"/summaryThighDecisionTree.R",sep = ""))
      comb<-summary_thigh(comb,win=win)
      colnames(comb$mat)<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
                            "class.Sit","class.Lay","class.Standmov","class.Walk","class.Run","Class.Stairs","Class.Cycle",
                            "Sleep","Non-wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max','mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
                            "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
                            "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
                            "nw.mg","nw.mgvm")
    }
    if(Classifier%in%"Only do enmo"){
      #  # source(paste(mypath,"/summaryenmo.R",sep = ""))
      comb<-summary_enmo(comb,win=win)
      colnames(comb$mat)<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
                            "Sleep","Nap","Non-wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max',
                            'mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
                            "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
                            "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
                            "nw.mg","nw.mgvm")
    }


    ifelse(!dir.exists(file.path(paste(output, "/scored files",folder_name,"/summary report",sep=""))),
           dir.create(file.path(paste(output, "/scored files",folder_name,"/summary report",sep=""))), FALSE)
    temp<-list(Fs=classifier$FS,model=Classifier)
    temp2<-list(axis=intensity$axis,Sed=intensity$Sed,Mod=intensity$Mod,Vig=intensity$Vig)
    all<-list(file=comb$allday,sampling_frequency=classifier$FS,epoch=win,classifier.info=temp,count.info=temp2,cal=ag)
    save(all, file=paste(output,"/scored files",folder_name,"/summary report","/",ID,file_name,".RData",sep=""))

    ifelse(!dir.exists(file.path(paste(output, "/scored files",folder_name,"/summary report/summary",sep=""))),
           dir.create(file.path(paste(output, "/scored files",folder_name,"/summary report/summary",sep=""))), FALSE)
    write.csv(comb$mat,file=paste(output,"/scored files",folder_name,"/summary report/summary","/",ID,".csv",sep=""),row.names = F)

    if(Classifier%in%"Only do enmo"){
      ifelse(!dir.exists(file.path(paste(output, "/scored files",folder_name,"/enmo csv",sep=""))),
             dir.create(file.path(paste(output, "/scored files",folder_name,"/enmo csv",sep=""))), FALSE)
      write.csv(all$file,file=paste(output,"/scored files",folder_name,"/enmo csv","/",ID,".csv",sep=""),row.names = F)
    }
    #Generate summary report with guide single file----
    # if(Guide==TRUE){ May remove entirely if Guide feature is not used anymore
    # cat(paste("\nChecking For Partial Day Summary\n"))
    # ifelse(!dir.exists(file.path(paste(output, "/scored files",folder_name,"/summary report_partial day",sep=""))),
    #        dir.create(file.path(paste(output, "/scored files",folder_name,"/summary report_partial day",sep=""))), FALSE)
    # a<-see[[2]]
    # a<-a[grepl(part$ID[i],a$ID),]
    # if(nrow(a)>0){
    #   cat(paste("\nNumber Of Days with Partial Day Analysis:",nrow(a),"\n"))
    #   hold<-list()
    #   if(Classifier%in%"Preschool Wrist Random Forest"|Classifier%in%"Preschool Hip Random Forest"|Classifier%in%"Preschool Wrist Random Forest Free Living"|Classifier%in%"School age Wrist Random Forest"|Classifier%in%"School age Hip Random Forest"|Classifier%in%"Preschool Hip Random Forest Free Living"){
    #    for(partial in 1:nrow(a)){
    #
    #      b<-a[partial,]
    #     dat<-paste(b$`Start Date`,b$Start)
    #     dat<-unlist(strsplit(dat,split=' ',fixed=TRUE))
    #     dat<-paste(dat[1],dat[3])
    #     start <- strptime(dat,format="%Y-%m-%d %H:%M:%S")
    #     dat<-paste(b$`End Date`,b$End)
    #     dat<-unlist(strsplit(dat,split=' ',fixed=TRUE))
    #     dat<-paste(dat[1],dat[3])
    #     stop <- strptime(dat,format="%Y-%m-%d %H:%M:%S")
    #     comb<-cbind(classifier$Activity,intensity=intensity$count[,5])
    #     ts<-strptime(paste(comb$date,comb$time),format="%Y-%m-%d %H:%M:%S")
    #     e<-which(ts>=start & ts<=stop)
    #     comb<-comb[e,]
    #     comb<-partialdaysummary(comb,win=win)
    #     colnames(comb$mat)<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
    #                           "class.Sed","class.Light","class.MV","class.Walk","class.Run",
    #                           "Sleep","Non-wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max','mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
    #                           "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
    #                           "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
    #                           "nw.mg","nw.mgvm")
    #     comb<-data.frame(comb$mat)
    #     #comb<-comb[,c(1:17,22:26)]
    #     comb$Start.time<-start;comb$End.time<-stop
    #     hold[[partial]]<-comb
    #     cat(paste("\nPartial Day Analysis Completed For:",start,"-",stop,"\n"))
    #
    #
    #    }
    #   }
    #   if(Classifier%in%"Ellis Wrist RF"|Classifier%in%"Ellis Hip RF"){
    #      # source(paste(mypath,"/partialdaysummaryellis.R",sep = ""))
    #     for(partial in 1:nrow(a)){
    #
    #       b<-a[partial,]
    #       dat<-paste(b$`Start Date`,b$Start)
    #       dat<-unlist(strsplit(dat,split=' ',fixed=TRUE))
    #       dat<-paste(dat[1],dat[3])
    #       start <- strptime(dat,format="%Y-%m-%d %H:%M:%S")
    #       dat<-paste(b$`End Date`,b$End)
    #       dat<-unlist(strsplit(dat,split=' ',fixed=TRUE))
    #       dat<-paste(dat[1],dat[3])
    #       stop <- strptime(dat,format="%Y-%m-%d %H:%M:%S")
    #       comb<-cbind(classifier$Activity,intensity=intensity$count[,5])
    #       ts<-strptime(paste(comb$date,comb$time),format="%Y-%m-%d %H:%M:%S")
    #       e<-which(ts>=start & ts<=stop)
    #       comb<-comb[e,]
    #       comb<-partialdaysummaryellis(comb,win=win)
    #       colnames(comb$mat)<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
    #                             "class.biking","class.sed","class.standmov","class.standstill","class.walk",
    #                             "Sleep","Non-wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max','mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
    #                             "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
    #                             "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
    #                             "nw.mg","nw.mgvm","class.vehicle")
    #       comb<-data.frame(comb$mat)
    #       #comb<-comb[,c(1:17,22:26)]
    #       comb$Start.time<-start;comb$End.time<-stop
    #       hold[[partial]]<-comb
    #       cat(paste("\nPartial Day Analysis Completed For:",start,"-",stop,"\n"))
    #
    #
    #     }
    #   }
    #   #if(Classifier%in%"Trost Adult Wrist RF"){
    #    #  # source(paste(mypath,"/partialdaysummaryellis.R",sep = ""))
    #    # for(partial in 1:nrow(a)){
    #    #
    #     #   Need to update with partial day summary for Trost Adult Wrist RF + St Laurence AG and AX RF + DT
    #     #}
    #   #}
    #   hold<-do.call(rbind,hold)
    #   colnames(hold)<-paste("p.",colnames(hold),sep="")
    #   write.csv(hold,file=paste(output,"/scored files",folder_name,"/summary report_partial day","/",ID,"partial day.csv",sep=""),row.names = F)
    #   orig<-read.csv(paste(output,"/scored files",folder_name,"/summary report/summary","/",ID,".csv",sep=""))
    #   temp<-list()
    #   for(zz in 1:nrow(orig)){
    #     orig1<-orig[zz,]
    #     for(zzz in 1:nrow(hold)){
    #     if(orig1$Date%in%hold$p.Date[zzz]){
    #       orig1<-cbind(orig1,hold[zzz,])
    #       orig1$p.tot.time<-(as.numeric(orig1$p.End.time)-as.numeric(orig1$p.Start.time))/60
    #       orig1$p.Start.time<-as.character(orig1$p.Start.time)
    #       orig1$p.End.time<-as.character(orig1$p.End.time)
    #       orig1[,44]<-"Partial Day"
    #     }
    #
    #     }
    #    temp[[zz]]<-orig1
    #   }
    #
    #   temp<-rbindlist(temp, fill = TRUE)
    #   temp[] <- lapply(temp, function(x) if(is.factor(x)) as.character(x) else x)
    #   temp[is.na(temp)] <- "."
    #   write.csv(temp,paste(output,"/scored files",folder_name,"/summary report/summary","/",ID,".csv",sep=""),row.names = F)
    # }
    # }

    #Generate visualisation plots----
    if(visual==T){
      ifelse(!dir.exists(file.path(paste(output, "/scored files",folder_name,"/daily plots",sep=""))),
             dir.create(file.path(paste(output, "/scored files",folder_name,"/daily plots",sep=""))), FALSE)
      if(!Classifier%in%"Only do enmo"){
        comb<-cbind(classifier$Activity,intensity=intensity$count[,5])
      }else{
        comb<-all$file
      }
      if(Classifier%in%"Preschool Wrist Random Forest Free Living Lag-Lead"|Classifier%in%"Preschool Hip Random Forest Free Living Lag-Lead"|Classifier%in%"Preschool Wrist Random Forest Free Living"|Classifier%in%"School age Wrist Random Forest"|Classifier%in%"School age Hip Random Forest"|Classifier%in%"Preschool Hip Random Forest Free Living"){
        GenPlot_general(comb,window=win,output=output,folder_name=folder_name,file_name=file_name)
      }
      if(Classifier%in%"Ellis Wrist RF"|Classifier%in%"Ellis Hip RF"){
         # source(paste(mypath,"/genplotellis.R",sep = ""))
        GenPlot_Ellis(comb,window=win,output=output,folder_name=folder_name,file_name=file_name)
      }
      if(Classifier%in%"Trost Adult Wrist RF"){
        #  # source(paste(mypath,"/genplottrostadultrf.R",sep = ""))
        GenPlot_TrostAdult(comb,window=win,output=output,folder_name=folder_name,file_name=file_name)
      }
      if(Classifier%in%"Thigh Decision Tree"){
        #  # source(paste(mypath,"/genplotThighDecisionTree.R",sep = ""))
        GenPlot_thigh(comb,window=win,output=output,folder_name=folder_name,file_name=file_name)
      }
      if(Classifier%in%"Only do enmo"){
        #  # source(paste(mypath,"/genplotenmo.R",sep = ""))
        GenPlot(comb,window=win,output=output,folder_name=folder_name,file_name=file_name)
      }
    }
    #Finished processing file----
    cat(paste("\n\n_______________Completed File:",ID,"_______________\n\n"))

    #progress.bar$step()
  }
  #Generate summary report all files----
  report<-Sys.glob(paste(output,"/scored files",folder_name,"/summary report/summary","/*.csv",sep=""))
  hold<-list()
  for(i in 1:length(report)){
    temp<-read.csv(report[i])
    hold[[i]]<-temp
  }
  if(Guide==TRUE){
    # report<-do.call(rbind.fill,hold) May remove if not used anymore
    # #Converting these columns to numeric
    # if(Classifier%in%"Preschool Wrist Random Forest"|Classifier%in%"Preschool Hip Random Forest"|Classifier%in%"Preschool Wrist Random Forest Free Living"|Classifier%in%"School age Wrist Random Forest"|Classifier%in%"School age Hip Random Forest"|Classifier%in%"Preschool Hip Random Forest Free Living"){
    # report[,c(5:19,22,25:28,31,38:42,46:61,64,67:70,73,80:84,87)] <-
    #   lapply(report[,c(5:19,22,25:28,31,38:42,46:61,64,67:70,73,80:84,87)], function(x) as.numeric(as.character(x)))
    # }
    # if(Classifier%in%"Ellis Wrist RF"|Classifier%in%"Ellis Hip RF"){
    #   report[,c(5:19,22,25:28,31,38:43,47:62,65,68:71,74,81:86,89)] <-
    #     lapply(report[,c(5:19,22,25:28,31,38:43,47:62,65,68:71,74,81:86,89)], function(x) as.numeric(as.character(x)))
    # }
    #if(Classifier%in%"Trost Adult Wrist RF"){
    # Need to update for Trost Adult Wrist RF model + St Laurence AG and AX RF
    #}
  }else{
    report<-do.call(rbind.fill,hold)
    if(Classifier%in%"Preschool Wrist Random Forest Free Living Lag-Lead"|Classifier%in%"Preschool Hip Random Forest Free Living Lag-Lead"|Classifier%in%"Preschool Wrist Random Forest Free Living"|Classifier%in%"School age Wrist Random Forest"|Classifier%in%"School age Hip Random Forest"|Classifier%in%"Preschool Hip Random Forest Free Living"){
      samp<-data.frame(matrix(NA,1,89))
      z<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
           "class.Sed","class.Light","class.MV","class.Walk","class.Run",
           "Sleep","Sleep.Period","Non.wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max','mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
           "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
           "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
           "nw.mg","nw.mgvm")
      colnames(samp)<-c(z,paste("p.",z,sep = ""),"p.Start.time","p.End.time","p.tot.time")
    }
    if(Classifier%in%"Ellis Wrist RF"|Classifier%in%"Ellis Hip RF"){
      samp<-data.frame(matrix(NA,1,91))
      z<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
           "class.bike","class.sed","class.standmov","class.standstill","class.walk",
           "Sleep","Sleep.Period","Non.wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max','mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
           "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
           "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
           "nw.mg","nw.mgvm","class.vehicle")
      colnames(samp)<-c(z,paste("p.",z,sep = ""),"p.Start.time","p.End.time","p.tot.time")
    }
    if(Classifier%in%"Trost Adult Wrist RF"){
      samp<-data.frame(matrix(NA,1,87))
      z<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
           "class.sed","class.stationaryplus","class.walk","class.run",
           "Sleep","Sleep.Period","Non.wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max','mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
           "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
           "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
           "nw.mg","nw.mgvm")
      colnames(samp)<-c(z,paste("p.",z,sep = ""),"p.Start.time","p.End.time","p.tot.time")
    }
    if(Classifier%in%"Thigh Decision Tree"){
      samp<-data.frame(matrix(NA,1,91))
      z<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
           "class.Sit","class.Lay","class.Standmov","class.Walk","class.Run","Class.Stairs","Class.Cycle",
           "Sleep","Non-wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max','mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
           "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
           "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
           "nw.mg","nw.mgvm")
      colnames(samp)<-c(z,paste("p.",z,sep = ""),"p.Start.time","p.End.time","p.tot.time")
    }
    if(Classifier%in%"Only do enmo"){
      samp<-data.frame(matrix(NA,1,81))
      z<-c("File","Date","Day","Weekday","int.Sed","int.Light","int.Mod","int.Vig",
           "Sleep","Sleep.Period","Nap","Non.wear","mvpa.b",'mvpa.bouts','mvpa.b.avg','mvpa.b.max','mvpa.b.max.st','mvpa.b.max.end','mvpa.b.min','mvpa.b.min.st','mvpa.b.min.end',
           "sed.b",'sed.bouts','sed.b.avg','sed.b.max','sed.b.max.st','sed.b.max.end','sed.b.min','sed.b.min.st','sed.b.min.end',
           "sleep.start","sleep.start.day","sleep.end","sleep.end.day","motionless","light.s","restless",
           "nw.mg","nw.mgvm")
      colnames(samp)<-c(z,paste("p.",z,sep = ""),"p.Start.time","p.End.time","p.tot.time")
    }
    report<-rbind.fill(report,samp)
    report<-report[-nrow(report),]
  }

  #Converting NaN to NA
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  #Locating NA's and replacing with "."
  report[is.nan(report)] <- NA
  report[is.na(report)] <- '.'
  names(report) <- gsub(x = names(report), pattern = "\\.", replacement = "_")
  ifelse(!dir.exists(file.path(paste(output, "/scored files",folder_name,"/summary report/report",sep=""))),
         dir.create(file.path(paste(output, "/scored files",folder_name,"/summary report/report",sep=""))), FALSE)
  write.csv(report,file=paste(output,"/scored files",folder_name,"/summary report/report/summary",file_name,".csv",sep=""),row.names = F)


  unlink(paste(output, "/scored files",folder_name,"/counts",sep=""), recursive = TRUE)
  unlink(paste(output, "/scored files",folder_name,"/raw",sep=""), recursive = TRUE)
  unlink(paste(output, "/scored files",folder_name,"/summary report_partial day",sep=""), recursive = TRUE)
  unlink(paste(output, "/scored files",folder_name,"/summary report/summary",sep=""), recursive = TRUE)
  options(warn = oldw)

  cat("\n\n-------All Files Have Been Summarized-------","\n\n")

}


