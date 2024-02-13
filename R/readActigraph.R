#' Reads ActiGraph Files
#'
#' @description
#' Function to read ActiGraph Files (support gt3x+, wgt3xBT, and Link devices)
#'
#' @param file Character with full path to actigraph file to read
#' @param verbose Logical (default = TRUE) indicating whether progress messages should
#' be printed in the console.
#'
#' @return List containing header info, accelerometer raw data, start time, and
#' sampling frequency for the measurement
#' @export
#'
readActigraph = function(file, verbose = TRUE) {
  # INTERNAL FUNCTIONS ------------------------------------------------------
  parse_gt3x_info = function(info, verbose = FALSE, ...) {
    # Function to parse txt info from gt3x data
    # info: info to parse
    # verbose: whether to print progress messages in console
    if (verbose) cat("\n  Parsing info.txt")
    # Read text file and assemble data frame
    meta = readLines(info)
    meta = strsplit(meta, ": ")
    meta_names = unlist(
      lapply(meta, function(x) x[1])
    )
    meta_names = gsub(" ", "_", meta_names)
    meta = data.frame(
      t(unlist(lapply(meta, function(x) x[2]))),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    names(meta) = meta_names
    # Format data frame
    num_vars = c("Battery_Voltage", "Sample_Rate", "Board_Revision",
                 "Unexpected_Resets", "Acceleration_Scale",
                 "Acceleration_Min", "Acceleration_Max")
    stopifnot(all(num_vars %in% names(meta)))
    for (i in num_vars) meta[ ,i] = as.numeric(as.character(meta[ ,i]))

    tick_vars = c("Start_Date", "Stop_Date", "Last_Sample_Time", "Download_Date")
    stopifnot(all(tick_vars %in% names(meta)))
    for (j in tick_vars) meta[ ,j] = tick_to_posix(meta[ ,j])

    meta$Download_Date = strftime(x = meta$Download_Date, format = "%m/%d/%Y")
    # return
    if (verbose) cat("  ............. COMPLETE")
    return(meta)
  }
  # MAIN FUNCTION -----------------------------------------------------------
  log.con = unz(file, "log.bin", open = "rb")
  info.con = unz(file, "info.txt")
  info = parse_gt3x_info(info.con)
  close(info.con)
  accScale = info$Acceleration_Scale
  sf = info$Sample_Rate
  deviceType = info$Device_Type
  # read data
  x = 1
  data = list()
  time = list()
  s = 0
  # First byte is the separator
  while (length(aa <- readBin(log.con, integer(), size = 1)) > 0) {
    if (aa == 30) {
      # Record type - 1 byte
      type = readBin(log.con, integer(), size = 1)

      # Unix timestamp - 4 bytes
      ts = readBin(log.con, integer(), size = 4)

      # Size of payload (in bytes) - 2 bytes
      size = readBin(log.con, integer(), size = 2)
      if (length(type) == 0) type = 99L # added 11/07/2019 to account for empty values when plugged in to ActiLife
      if (length(size) == 0 || size < 0) size = 1L  #added 17/09/2019 to account for empty values when plugged in to ActiLife
      if (size == 1 & type == 0) {
        aa = 100
        s = 1
        next
      }
      # Link: type 26 is ACTIVITY; wgt3xB: type 0 is ACTIVITY
      if (deviceType == "Link") ACTIVITY = 26L else ACTIVITY = 0L
      if (type == ACTIVITY & !length(size) == 0) {
        if (x == 1) prevTs = ts - 1
        # For missing samples - impute last yxz values
        if (ts - prevTs != 1 & !ts - prevTs <= 0) {
          if (s == 1) {
            while (ts - prevTs != 1) {
              data[[x]] = rep(0, sf*3)
              prevTs = prevTs + 1
              time[[x]] = prevTs
              x = x + 1
            }
            s = 0
          } else {
            while (ts - prevTs != 1) {
              data[[x]] = data[[x - 1]]
              prevTs = prevTs + 1
              time[[x]] = prevTs
              x = x + 1
            }
          }
        }

        if (deviceType != "Link") {
          #if(ts-prevTs<=0){break}
          data[[x]] = read_activityC(readBin(log.con, raw(), size), accScale)
          time[[x]] = ts
          prevTs = ts
          x = x + 1
        } else {
          see1 = readBin(log.con, "raw", size,endian = 'little')
          see2 = DescTools::HexToDec(see1)
          a = matrix(see2, nrow = ceiling(length(see2)/6), ncol = 6, byrow = T)
          ########
          a[which(a[,4] < 10), 3] = (a[which(a[,4] < 10), 3]/accScale) + a[which(a[, 4] < 10), 4]
          a[which(a[,4] > 100), 3] = ((a[which(a[,4] > 100),3] - accScale)/accScale) + (a[which(a[,4] > 100), 4] - 255)
          a[which(a[,4] == 90), 3] = 0
          ###########
          a[which(a[,2] < 10), 1] = (a[which(a[,2] < 10), 1]/accScale) + a[which(a[, 2] < 10), 2]
          a[which(a[,2] > 100), 1] = ((a[which(a[,2] > 100),1] - accScale)/accScale) + (a[which(a[,2] > 100),2] - 255)
          a[which(a[,2] == 90), 1] = 0
          ###########
          a[which(a[,6] < 10),5] = (a[which(a[,6] < 10),5]/accScale) + a[which(a[, 6] < 10), 6]
          a[which(a[,6] > 100),5] = ((a[which(a[,6] > 100),5] - accScale)/accScale) + (a[which(a[, 6] > 100), 6] - 255)
          a[which(a[,6] == 90),5] = 0
          ########
          data[[x]] = a[,c(1,3,5)]
          time[[x]] = ts
          prevTs = ts
          x = x + 1
          # cat("Reading In Hour:",round((x/60/60),2),"\r")
        }
      } else {
        suppressWarnings( # avoid embedded nuls warning
          readChar(log.con, size, useBytes = TRUE)
        )
      }
      # Checksum - 1 byte
      readBin(log.con, integer(), size = 1)
    }
    if (length(data) > (60*60*24*28)) break
  }
  if (length(data) > (60*60*24*28)) {
    cat("\nKeeping first 28 days of data\n")
    data = data[1:((60*60*24*28) + 1)]
  }
  # data
  if (deviceType == "Link") {
    data = do.call(rbind, data)
  } else if (deviceType != "Link") {
    data = matrix(unlist(data), ncol = 3, byrow = TRUE)
    data = data[, c(2, 1, 3)] # wgt3xb read in yxz order
  }
  colnames(data) = c("x", "y", "z")
  close(log.con)
  # start time
  start_time = strptime(format(info$Start_Date,"%Y-%m-%d %H:%M:%OS"), "%Y-%m-%d %H:%M:%OS")
  start_time = as.numeric(start_time)
  # return
  return(invisible(list(header = info, data = data, start_time = start_time, sf = sf)))
}
