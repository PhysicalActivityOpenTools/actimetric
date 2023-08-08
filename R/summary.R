#' Title
#'
#' @param ag
#'
#' @return
#' @export
#'
#' @examples
summary_general1<-function(ag){
  day<-weekdays(as.Date(ag$date)) #this will be used when wanting to summarise data by day of week
  ag<-cbind(ag,day)
  ag<-ag[,c(1:3,32:33)]
  a<-unique(ag$date)
  mat<-matrix("",length(a),10)
  see<-list()

  for(i in 1:length(a)){
    final<-ag[ag$date%in%a[i]]
    sed<-as.numeric(length(which(final$class==1)))*15/60
    light<-as.numeric(length(which(final$class==2)))*15/60
    walk<-as.numeric(length(which(final$class==3)))*15/60
    run<-as.numeric(length(which(final$class==4)))*15/60
    mv<-as.numeric(length(which(final$class==5)))*15/60
    nonwear<-as.numeric(length(which(final$class==7)))*15/60
    see[[i]]<-list(sed = sed,light = light,walk = walk,run = run,mv = mv,nonwear = nonwear,date = final$date[1], day = final$day[1],ID = final$subject[1])

  }


  mat[1,1]<-"ID";mat[1,2]<-"Date";mat[1,3]<-"Day";mat[1,4]<-"Weekend";mat[1,5]<-"Sed";mat[1,6]<-"Light";mat[1,7]<-"Walk";mat[1,8]<-"Run";mat[1,9]<-"MV";mat[1,10]<-"Nonwear"
  z<-0
  for(i in 1:length(a)){
    z<-z+1
    aa<-see[[i]]
   mat[z,1]<-aa$ID;mat[z,2]<-aa$date;mat[z,3]<-aa$day;mat[z,4]<-aa$day;mat[z,5]<-aa$sed;mat[z,6]<-aa$light;mat[z,7]<-aa$walk;mat[z,8]<-aa$run;mat[z,9]<-aa$mv;mat[z,10]<-aa$nonwear
   mat[z,4]<-ifelse(mat[z,4]%in%c("Saturday","Sunday"),0,1)

   }

  return(mat)
}


