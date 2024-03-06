#' Reads activPAL raw acceleration files
#'
#' @param file Character with path to raw acceleration file
#'
#' @return Data frame with acceleration data
#' @export
#' @importFrom data.table fread
read.activpal = function(file) {
  data = data.table::fread(file, data.table = F)
  data = data[,c(-2)]
  data$Time = as.POSIXlt(data$Time*(60*60*24) + as.POSIXlt("1899-12-30"))
  data$X = sapply(data$X,FUN = function(x) (x - 128)/64)
  data$Y = sapply(data$Y,FUN = function(x) (x - 128)/64)
  data$Z = sapply(data$Z,FUN = function(x) (x - 128)/64)
  return(data)
}
