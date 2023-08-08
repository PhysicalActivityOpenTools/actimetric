#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
note<-function(x,y){
  cat("\n______________________________________________________________________________________________________\n")
  cat("\nCreated on 22/02/2019\n")
  if(x%in%"Preschool Random Forest"){
  cat("\nActvity Classifier Publication: \nTrost et al., Medicine and Science in Sport and Exercise 2018;50(3): 634-641 \n")
  }
  if(x%in%"Ellis Wrist RF"|x%in%"Ellis Hip RF"){
    cat("\nActvity Classifier Publication: \nEllis et al., Medicine and Science in Sport and Exercise 2016;48(5): 933-940 \n")
  }
  if(x%in%"Trost Adult Wrist RF"){
    cat("\nActvity Classifier Publication: \nPavey et al., Journal of Science and Medicine in Sport 2017;20(1): 75-80 \n")
  }
  if(y==420){
  cat("\nIntensity Cut-Point Publication: \nPate et al., Obesity 2006;14(11): 2000-2006 \n")
  }
  if(y==100.6|y==93.2|y==69.1|y==68.7|y==142.6|y==152.8|y==201.4|y==191.6){
    cat("\nIntensity Cut-Point Publication: \nHildebrand et al., Medicine and Science in Sport and Exercise 2014;46(9): 1816-1824 \n")
  }
  if(y==1952){
    cat("\nIntensity Cut-Point Publication: \nFreedson et al., Medicine and Science in Sport and Exercise 1998;30(5): 777-781 \n")
  }
  cat("\n______________________________________________________________________________________________________\n")
}
