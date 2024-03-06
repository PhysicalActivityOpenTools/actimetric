#' Read Accelerometer Raw Data File
#'
#' @description
#' Function aimed to read accelerometer raw data. At the moment,
#' the function supports ActiGraph gt3x and csv data, GENEActiv bin
#' and csv data
#'
#' @param file Character with full path to accelerometer raw data file
#' @param verbose Logical (default = TRUE) indicating whether progress messages should
#' be printed in the console.
#'
#' @return List containing header info, accelerometer raw data, start time and ID
#' for the file.
#' @export
#' @import Rcpp
ReadAccFile = function(file, verbose = TRUE) {
  # extract ID and file extension (format) ------------
  dot_position = regexpr("\\.([[:alnum:]]+)$", file)
  format = substr(file, dot_position + 1, nchar(file))
  ID = gsub(paste0(".", format, "$"), "", basename(file))
  if (verbose) {
    cat(paste("\nReading", format, "accelerometer data...\n"))
    cat(paste("File Size:", round(file.info(file)$size/1024^2, 1), "MB"))
  }
  # Read data --------------------------
  if (format %in% c("gt3x", "GT3X")) {
    raw = readActigraph(file)
  } else if (format == "csv") {
    raw = readCsv(file)
    if (raw$mon == "geneactiv") colnames(raw$data) = c("x", "y", "z", "temp")
    if (raw$mon %in% c("actigraph", "activpal")) colnames(raw$data) = c("x", "y", "z")
  } else if (format == "bin") {
    raw = readGeneactiv(file)
    raw$header = data.frame(1, 1)
    zz = GENEAread::header.info(file)
    raw$sf = as.numeric(unlist(zz$Value[2]))
  } else if (format == "cwa") {
    raw = readax3(file) #2020/19/11 AX3 has temp, battery, and light data. These have been removed
    #to save memory space and speed up reading in data. Can be included in the future if needed
    raw$header = data.frame(1,1)
  }
  # Add identifier to raw
  raw$ID = ID
  return(raw)
}
