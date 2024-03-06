# read_acceleration <- function(fid, accScale,Fs) {
#   cat("\n")
#   x <- 1
#   data <- list()
#   time <- list()
#   s<-0
#   # First byte is the seperator
#   while (length(aa<-readBin(fid, integer(), size = 1)) > 0) {
#     if(aa==30){
#       # Record type - 1 byte
#       type <- readBin(fid, integer(), size = 1)
#
#       # Unix timestamp - 4 bytes
#       ts <- readBin(fid, integer(), size = 4)
#
#       # Size of payload (in bytes) - 2 bytes
#       size <- readBin(fid, integer(), size = 2)
#       if(length(type)==0){type=99L} #added 11/07/2019 to account for empty values when plugged in to ActiLife
#       if(length(size)==0||size<0){size=1L}  #added 17/09/2019 to account for empty values when plugged in to ActiLife
#       if(size==1 & type==0){
#          aa<-100
#         s<-1
#         #try(while(!aa==30){
#         #aa<-readBin(fid, integer(), size = 1)
#         #},silent = T)
#         next
#       }
#       # Type 0 is ACTIVITY
#       if (type == 0L & !length(size)==0) {
#          if (x==1) prevTs <- ts - 1
#
#         # For missing samples - impute last yxz values
#         if (ts - prevTs != 1 & !ts-prevTs<=0)
#           if(s==1){
#             while (ts - prevTs != 1) {
#               data[[x]] <- rep(0,Fs*3)
#               prevTs <- prevTs + 1
#               time[[x]] <- prevTs
#               x <- x + 1
#             }
#             s<-0
#           }else{
#             while (ts - prevTs != 1) {
#               data[[x]] <- data[[x-1]]
#               prevTs <- prevTs + 1
#               time[[x]] <- prevTs
#               x <- x + 1
#             }}
#         #if(ts-prevTs<=0){break}
#         data[[x]] <- read_activityC(readBin(fid, raw(), size), accScale)
#         time[[x]] <- ts
#         prevTs <- ts
#         x <- x + 1
#
#         # cat("Reading In Hour:",round((x/60/60),2),"\r")
#       } else {
#         readChar(fid, size, useBytes = TRUE)
#       }
#       # Checksum - 1 byte
#       readBin(fid, integer(), size = 1)
#     }
#     if(length(data)>(60*60*24*28)){break}
#
#     }
#   if(length(data)>(60*60*24*28)){
#     cat("\nKeeping first 28 days of data\n")
#     data<-data[1:((60*60*24*28)+1)]
#   }
#
#   list(data = matrix(unlist(data), ncol = 3, byrow = TRUE), dat = time[[1]])
# }
#
#
#
