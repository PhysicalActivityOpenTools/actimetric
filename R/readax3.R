#' Title
#'
#' @param raw
#'
#' @return
#' @export
#'
#' @examples
readax3<-function(raw){

  updatepageindexing = function(startpage=c(), deltapage=c(), blocknumber=c(),PreviousEndPage=c(),

                                mon=c(),dformat=c()) {


    if (blocknumber != 1 & length(PreviousEndPage) != 0) {

      if ((mon == 2 & dformat == 1) | dformat == 2) {
        startpage = PreviousEndPage + 1

      }

    }

    endpage = startpage + deltapage

    return(list(startpage=startpage,endpage=endpage))
  }


  PP<- cwa(raw,start=1,end=10,desiredtz = "UTC")
  sf<- PP$h$frequency
  chunksize = 1
  blocksize = round (14512*(sf/50)*chunksize)
  blocksize = round(blocksize*1.0043)
  blocknumber=1
  PreviousEndPage=0

  startpage = blocksize*(blocknumber-1)

  deltapage = blocksize
  LD=0
  i=1
  while(LD<1){
    if (i == 1) {
      cat(paste("\nReading in page: ", i, sep = ""))
    }
    else {
      cat(paste(" ", i, sep = ""))
    }
     UPI = updatepageindexing(startpage=startpage,deltapage=deltapage,

                             blocknumber=blocknumber,PreviousEndPage=PreviousEndPage, mon=2, dformat=1)

    startpage = UPI$startpage;    endpage = UPI$endpage


    try(expr={P = cwa(raw,start=startpage,end = endpage,desiredtz = 'UTC')},silent=TRUE)


    endpage-startpage
    blocknumber=blocknumber+1
    PreviousEndPage=endpage

    if(i==1){
      matt<-P$data[,1:3]

    }
    if(i>1){
      matt<-rbind(matt,P$data[,1:3])

    }
    p<-nrow(P$data)
    if(length(p)==0){p<-0}
    if (p < (sf*60*2+1)) {

      LD<- 2
    }

    i<-i+1
  }
  start_time = as.numeric(strptime(PP$header$start,"%Y-%m-%d %H:%M:%OS"))
  cat('\n')
  return(invisible(list(sf = sf, data = matt,start_time = start_time)))

}


