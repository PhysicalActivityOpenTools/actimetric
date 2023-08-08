#' Title
#'
#' @param file.and.path
#'
#' @return
#' @export
#'
#' @examples
read.accel<-function(file.and.path)
{
  header=FALSE
  options(warn = -1)
  data = read.csv(paste(file.and.path,sep=""),nrow=10,skip=10)
  if(ncol(data)==1){ #it is an activpal
    mon=3
  }
  if (ncol(data) == 2) { #it is a geneactivefile
    mon = 1
  }
  if(ncol(data)>2)  {	#it is an actigraph file
    mon = 2
  }

  #read Fs from header
  if (mon == 1) {
    data = read.csv(paste(file.and.path,sep=""),nrow=50,skip=0)
    Fs = as.character(data[which(as.character(data[,1]) == "Measurement Frequency"),2])
    Fs = as.numeric(unlist(strsplit(Fs," "))[1])
    dat = as.character(data[which(as.character(data[,1]) == "Start Time"),2])
    dat = as.POSIXlt(dat)
    data<-read.csv(file.and.path,skip = 100,nrow=1,header = F,skipNul = T)
    if (ncol(data)==7)
    {
      data<-fread(file.and.path,skip=100,nrows = Fs*60*60*24*12)
      options(warn = 0)
      data<-data[,c(2:4,7)]
      full.data<-
        data.frame(data)

      return(list(data = full.data, Fs = Fs, header = header, mon = mon, dat = as.numeric(dat) ))
    }
    else{

    }
  } else if (mon == 2) {
    data <- readLines(paste(file.and.path),n=10)
    Fs = unlist(strsplit(data[1],split="Hz",fixed=T))
    Fs<-unlist(strsplit(Fs[1],split=' ',fixed=T))
    Fs<-as.numeric(Fs[length(Fs)])
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
    data<-read.csv(file.and.path,skip = 10,nrow=1,header = F,skipNul = T)
    if(!class(data[,2])%in%"numeric"){
      header=TRUE
    }else{
      header=FALSE
    }

    if (ncol(data)==4)
    {
      full.data<-fread(file.and.path,skip=10,header = header,select = c(2:4),data.table = F)

      return(list(data = full.data, Fs = Fs, header = header,mon= mon, dat = as.numeric(dat) ))
    }
    else{
      full.data<-fread(file.and.path,skip=10,header = header,data.table = F)

      return(list(data = full.data, Fs = Fs, header = header, mon = mon, dat = as.numeric(dat) ))
    }
  }
  else if (mon ==3){
    data<- fread(file.and.path,data.table = F,select = c(3:5))
    m<-as.numeric(max(data))
    if(m>250 & m<260){
    data$X<-sapply(data$X,FUN = function(x)(x-128)/64)
    data$Y<-sapply(data$Y,FUN = function(x)(x-128)/64)
    data$Z<-sapply(data$Z,FUN = function(x)(x-128)/64)}
    dat<- fread(file.and.path,data.table = F,select = c(1),nrows=2)
    dat$Time<- as.POSIXlt(dat$Time*(60*60*24)+as.POSIXlt("1899-12-30"))
    Fs<-round(1/round(as.numeric(dat$Time[2]-dat$Time[1]),2))
    dat<-dat$Time[1]
    return(list(data = data, Fs = 20, header = header, mon = mon, dat = as.numeric(dat) ))
  }}


