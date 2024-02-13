# readAG_link <- function(fid, accScale,Fs) {
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
#         aa<-100
#         s<-1
#         #try(while(!aa==30){
#         #aa<-readBin(fid, integer(), size = 1)
#         #},silent = T)
#         next
#       }
#       # Type 26 is ACTIVITY
#       if (type == 26L & !length(size)==0) {
#         if (x==1) prevTs <- ts - 1
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
#         see1 <-readBin(fid, "raw", size,endian = 'little')
#         see2<-DescTools::HexToDec(see1)
#         a<-matrix(see2,nrow = ceiling(length(see2)/6),ncol = 6,byrow = T)
#         ########
#         a[which(a[,4]<10),3]<-(a[which(a[,4]<10),3]/accScale)+a[which(a[,4]<10),4]
#         a[which(a[,4]>100),3]<-((a[which(a[,4]>100),3]-accScale)/accScale)+(a[which(a[,4]>100),4]-255)
#         a[which(a[,4]==90),3]<-0
#         ###########
#         a[which(a[,2]<10),1]<-(a[which(a[,2]<10),1]/accScale)+a[which(a[,2]<10),2]
#         a[which(a[,2]>100),1]<-((a[which(a[,2]>100),1]-accScale)/accScale)+(a[which(a[,2]>100),2]-255)
#         a[which(a[,2]==90),1]<-0
#         ###########
#         a[which(a[,6]<10),5]<-(a[which(a[,6]<10),5]/accScale)+a[which(a[,6]<10),6]
#         a[which(a[,6]>100),5]<-((a[which(a[,6]>100),5]-accScale)/accScale)+(a[which(a[,6]>100),6]-255)
#         a[which(a[,6]==90),5]<-0
#         ########
#         a[,1]<-round(a[,1],3)
#         a[,3]<-round(a[,3],3)
#         a[,5]<-round(a[,5],3)
#         data[[x]] <- a[,c(1,3,5)]
#         time[[x]] <- ts
#         prevTs <- ts
#         x <- x + 1
#         # cat("Reading In Hour:",round((x/60/60),2),"\r")
#
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
#   list( data = do.call(rbind,data),dat = time[[1]])
# }
