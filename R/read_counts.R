#' Reads counts files
#'
#' @param file Character with path to raw acceleration file
#'
#' @return List containing counts data and epoch size
#' @export
read.counts = function(file) {

  header=FALSE
  options(warn = -1)

  # read header and start time
  data <- readLines(paste(file),n=10)
  dateFmt = stringr::str_match(data, "date format ([a-z,A-Z,/]*)")[1,2]
  dateFmt = gsub("yyyy", "%Y", dateFmt)
  dateFmt = gsub("MM", "%m", dateFmt)
  dateFmt = gsub("M", "%m", dateFmt)
  dateFmt = gsub("d", "%d", dateFmt)
  start.time <- data[3]
  start.time <- (strsplit(start.time,split=" ")[[1]][3])
  start.date <- data[4]
  start.date <- (strsplit(start.date,split=" ")[[1]][3])
  dat = strptime(paste(start.date, start.time), paste(dateFmt,
                                                      "%H:%M:%S"))
  epoch<-data[5]
  epoch<-unlist(strsplit(epoch,split=':',fixed=TRUE))
  epoch1<-as.numeric(epoch[length(epoch)-1])*60
  epoch<-as.numeric(epoch[length(epoch)])
  epoch<-epoch1+epoch
  data<-fread(file,skip = 10,nrow=1,header = F,data.table = F)
  if(class(data[1,2])%in%"character"){
    header=TRUE
  }else{
    header=FALSE
  }
  data<-fread(file,skip = 11,nrow=1,header = F,data.table = F)

  if (!class(data[,1])%in%"integer" &!class(data[,2])%in%"integer")
  {
    data<-fread(file,skip=10,header = header)
    data<-data[,c(3:5)]
    n<- dim(data)[1] #number of rows in data
    full.data<-
      data.frame(time = dat + epoch*(0:(n-1)),data)

    return(list(data = full.data, header = header))
  }
  if (!class(data[,1])%in%"integer" &class(data[,2])%in%"integer"){
    data<-fread(file,skip=10,header = header)
    data<-data[,c(2:4)]
    n<- dim(data)[1] #number of rows in data
    full.data<-
      data.frame(time = dat + epoch*(0:(n-1)),data)

    return(list(data = full.data, header = header ))
  }
  if (class(data[,1])%in%"integer"){
    data<-fread(file,skip=10,header = header)
    data<-data[,c(1:3)]
    n<- dim(data)[1] #number of rows in data
    full.data<-
      data.frame(time = dat + epoch*(0:(n-1)),data)

    return(list(data = full.data, epoch=epoch ))
  }

}
