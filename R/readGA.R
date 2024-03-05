#' Title
#'
#' @param raw
#'
#' @return
#' @export
#'
#' @examples
readGeneactiv<-function(raw){

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



  zz<-GENEAread::header.info(raw)
  sf<-as.numeric(unlist(zz$Value[2]))
  chunksize=1
  blocksize = round(14512 * (sf/50) * chunksize)
  blocknumber=1
  PreviousEndPage=0

  startpage = blocksize*(blocknumber-1) + 1

  deltapage = blocksize
  LD=0
  i=1
  while(LD<1){
    UPI = updatepageindexing(startpage=startpage,deltapage=deltapage,

                             blocknumber=blocknumber,PreviousEndPage=PreviousEndPage, mon=2, dformat=1)

    startpage = UPI$startpage;    endpage = UPI$endpage


    try(expr={P = GENEAread::read.bin(binfile=raw,
                                      start=startpage,

                                      end=endpage,calibrate=FALSE,do.temp=F,mmap.load=FALSE,verbose = F)},silent=TRUE)


    endpage-startpage
    blocknumber=blocknumber+1
    PreviousEndPage=endpage

    if(i==1){
      #matt<-P$data.out[,1:4]
      matt<-P$data.out[,2:4]

    }
    if(i>1){
      #matt<-rbind(matt,P$data.out[,1:4])
      matt<-rbind(matt,P$data.out[,2:4])

    }

    if (nrow(P$data.out) < (blocksize*300)) { #last block

      LD<- 2
    }
    i<-i+1
  }
  # dat = strptime(unlist(zz$Value[4]),"%Y-%m-%d %H:%M:%OS")
  #
  # time = dat + (0:(nrow(matt)-1))/sf
  start_time = as.numeric(strptime(unlist(zz$Value[4]),"%Y-%m-%d %H:%M:%OS"))


  return(invisible(list(data = matt,start_time = start_time)))

}


