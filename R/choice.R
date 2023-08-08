#' Title
#'
#' @return
#' @export
#'
#' @examples
choice <- function(){

  cat("Before Beginning, The Program Needs Your Input\n\n")
  #Sys.sleep(3)
  n1<-readline(prompt=cat("\n\nChoose Which Activity Classifier To Use:\n\n Preschool Wrist Random Forest Free Living\n Preschool Hip Random Forest Free Living\n Preschool Hip Random Forest Free Living Lag-Lead\n Preschool Wrist Random Forest Free Living Lag-Lead
\n School age Wrist Random Forest\n School age Hip Random Forest
 \n Trost Adult Wrist RF\n\n Ellis Wrist RF\n Ellis Hip RF\n\n Thigh Decision Tree\n\n Only do enmo" ))
  #n2<-readline(prompt=cat("\n\nApply Intensity Cut-Points:\nTRUE\nFALSE " ))
  if(n1%in%"Only do enmo"){
    n8<-readline(prompt=cat("\n\nEnmo window length in seconds: " ))
  }else{n8<-0}
  n2=TRUE
  if(n2==TRUE){
  n3<-readline(prompt=cat("\n\nIntensity Type:\nCounts\nRaw " ))
  n4<-readline(prompt=cat("\n\nSed Cut-Point: " ))
  n5<-readline(prompt=cat("\n\nMod Cut-Point: " ))
  n6<-readline(prompt=cat("\n\nVig Cut-Point: " ))
  n7<-readline(prompt=cat("\n\nWhich Axis To Use for Cut-Points:\nyaxis\nvm\nenmo\n " ))
  }
  if(n2==FALSE){
    n3<-0
    n4<-0
    n5<-0
    n6<-0
    n7<-0
  }
  cat("\n\nThank You\n\n")
  Sys.sleep(3)
  n1<-as.character(n1)
  n3<-as.character(n3)
  n4<-as.numeric(n4)
  n5<-as.numeric(n5)
  n6<-as.numeric(n6)
  n7<-as.character(n7)
  n8<-as.numeric(n8)



  c(n1,n2,n3,n4,n5,n6,n7,n8)
}


