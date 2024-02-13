#' Reads CSV Files With Accelerometer Data
#'
#' @param file Character with full path to accelerometer raw data file
#'
#' @return List containing header info, accelerometer raw data, sampling frequency,
#' monitor brand, and start time for the measurement
#' @export
#' @importFrom data.table fread
readCsv = function(file) {
  header = FALSE
  options(warn = -1)
  # identify monitor
  data = read.csv(file, nrow = 10, skip = 10)
  if (ncol(data) == 1) mon = "activpal" # it is an activpal
  if (ncol(data) == 2) mon = "geneactiv" # it is a geneactivefile
  if (ncol(data) > 2) mon = "actigraph"	# it is an actigraph file

  #read sf from header
  if (mon == "geneactiv") {
    data = read.csv(file, nrow = 50, skip = 0)
    sf = as.character(data[which(as.character(data[,1]) == "Measurement Frequency"), 2])
    sf = as.numeric(unlist(strsplit(sf," "))[1])
    start_time = as.character(data[which(as.character(data[,1]) == "Start Time"), 2])
    start_time = as.POSIXlt(start_time)
    data = read.csv(file, skip = 100, nrow = 1, header = F, skipNul = T)
    if (ncol(data) == 7) {
      data = data.table::fread(file, skip = 100, nrows = sf*60*60*24*12)
      options(warn = 0)
      data = data[, c(2:4,7)]
      full.data = data.frame(data)
      return(list(data = full.data, sf = sf, header = header, mon = mon, start_time = as.numeric(start_time) ))
    }
  } else if (mon == "actigraph") {
    data = readLines(paste(file),n = 10)
    sf = unlist(strsplit(data[1], split = "Hz", fixed = T))
    sf = unlist(strsplit(sf[1], split = ' ', fixed = T))
    sf = as.numeric(sf[length(sf)])
    dateFormat_match = regexpr("date format ([a-z,A-Z,/]*)", data)
    dateFormat = regmatches(data, dateFormat_match)
    dateFormat = gsub("date format ", "", dateFormat)
    dateFormat = gsub("yyyy", "%Y", dateFormat)
    dateFormat = gsub("MM", "%m", dateFormat)
    dateFormat = gsub("M", "%m", dateFormat)
    dateFormat = gsub("d", "%d", dateFormat)
    start.time = data[3]
    start.time = (strsplit(start.time, split = " ")[[1]][3])
    start.date = data[4]
    start.date = (strsplit(start.date, split = " ")[[1]][3])
    start_time = strptime(paste(start.date, start.time), paste(dateFormat, "%H:%M:%S"))
    data = read.csv(file, skip = 10, nrow = 1, header = F, skipNul = T)
    if (!class(data[,2]) %in% "numeric") header = TRUE else header = FALSE
    if (ncol(data) == 4) {
      full.data = data.table::fread(file, skip = 10, header = header, select = c(2:4), data.table = F)
    } else {
      full.data = data.table::fread(file, skip = 10, header = header, data.table = F)
    }
    return(list(data = full.data, sf = sf, header = header, mon = mon, start_time = as.numeric(start_time)))
  }
  else if (mon == "activpal") {
    full.data = data.table::fread(file, data.table = F, select = c(3:5))
    m = as.numeric(max(data))
    if (m > 250 & m < 260) {
      full.data$X = sapply(full.data$X, FUN = function(x) (x - 128)/64)
      full.data$Y = sapply(full.data$Y, FUN = function(x) (x - 128)/64)
      full.data$Z = sapply(full.data$Z, FUN = function(x) (x - 128)/64)
    }
    start_time = data.table::fread(file, data.table = F, select = c(1), nrows = 2)
    start_time$Time = as.POSIXlt(start_time$Time*(60*60*24) + as.POSIXlt("1899-12-30"))
    sf = round(1/round(as.numeric(start_time$Time[2] - start_time$Time[1]), 2))
    start_time = start_time$Time[1]
    return(list(data = full.data, sf = 20, header = header, mon = mon, start_time = as.numeric(start_time)))
  }
}


