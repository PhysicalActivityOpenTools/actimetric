#' Title
#'
#' @param fileName
#'
#' @return
#' @export
#'
#' @examples
read_gt3x <- function(fileName){

  info.con <- unz(fileName, "info.txt")
  log.con <- unz(fileName, "log.bin", open = "rb")

  info <-parse_info_txt(info.con,verbose = F)
  close(info.con)
  if(info$Device_Type%in%"Link"){
    data<-read_accelerationLink(log.con,info$Acceleration_Scale,info$Sample_Rate)
    close(log.con)
    dat = as.POSIXct(data$dat, origin = "1970-01-01",tz='UTC')
    dat = strptime(format(info$Start_Date,"%Y-%m-%d %H:%M:%OS"),"%Y-%m-%d %H:%M:%OS")
    dat<-as.numeric(dat)
    data <- cbind(data$data) #Link read in xyz order
  }else{
    data <- read_acceleration(log.con, info$Acceleration_Scale,info$Sample_Rate)
    close(log.con)
    # Create 13-digit unix timestamp to preserve milliseconds
    #Added 2021/05/28; when data is set to be collected but monitor is connected to USB after initialization time
    dat = as.POSIXct(data$dat, origin = "1970-01-01",tz='UTC') #Need this to keep timezone timestamp data was collected
    dat = strptime(format(dat,"%Y-%m-%d %H:%M:%OS"),"%Y-%m-%d %H:%M:%OS")
    dat<-as.numeric(dat)
    data <- cbind(data$data[,c(2, 1, 3)]) #gt3x read in yxz order
  }
  colnames(data) <- c("x", "y", "z")
  return(invisible(list(header = info, data = data,dat = dat)))
}
