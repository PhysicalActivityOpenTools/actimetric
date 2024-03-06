#' Read CWA files
#'
#' @param file Character with full path to accelerometer raw data file
#' @param start Numeric with starting page for reading file.
#' @param end Numeric with ending page for reading file.
#' @param progressBar Logical indicating whether to show a progress bar.
#' @param desiredtz Desired time zone.
#'
#' @return List with header and datar frame with acceleration data.
#' @export
cwa = function(file, start = 0, end = 0, progressBar = FALSE, desiredtz = c()) {
  timestampDecoder = function(coded, fraction, shift) {
    year = struc[[1]]
    if (year == 0) {
      year = bitwAnd(bitwShiftR(coded, 26), 63L) + 2000
      month = bitwAnd(bitwShiftR(coded, 22), 15L)
      day = bitwAnd(bitwShiftR(coded, 17), 31)
      hours = bitwAnd(bitwShiftR(coded, 12), 31L)
      mins = bitwAnd(bitwShiftR(coded, 6), 63L)
      secs = bitwAnd(coded, 63L)
      year = as.numeric(as.POSIXct(paste0(year, "-", month,
                                          "-", day, " ", hours, ":", mins, ":", secs),
                                   tz = desiredtz))
    }
    else {
      secs = bitwAnd(coded, 63L)
      oldSecs = struc[[2]]
      if (secs < oldSecs)
        oldSecs = oldSecs - 60
      year = year + (secs - oldSecs)
    }
    struc <<- list(year, secs)
    return(year + fraction/65536 + shift)
  }
  readHeader = function(fid, numDBlocks) {
    seek(fid, 0)
    idstr = readChar(fid, 2, useBytes = TRUE)
    if (idstr == "MD") {
      readChar(fid, 3, useBytes = TRUE)
      uniqueSerialCode = readBin(fid, integer(), size = 2)
      readChar(fid, 29, useBytes = TRUE)
      frequency = round(3200/bitwShiftL(1, 15 - bitwAnd(readBin(fid,
                                                                integer(), size = 1), 15)))
      readChar(fid, 4, useBytes = TRUE)
      version = readBin(fid, integer(), size = 1)
      readChar(fid, 982, useBytes = TRUE)
      datas = readDataBlock(fid, complete = FALSE)
      if (is.null(datas)) {
        stop("Error in the first data block reading")
      }
      if (frequency != datas$frequency) {
        warning("Inconsistent value of measurement frequency: there is ",
                frequency, " in header and ", datas$frequency,
                " in the first data block ")
      }
    }
    else {
      return(invisible(NULL))
    }
    return(invisible(list(uniqueSerialCode = uniqueSerialCode,
                          frequency = frequency, start = as.POSIXct(datas$start,
                                                                    origin = "1970-01-01", tz = desiredtz), device = "Axivity",
                          firmwareVersion = version, blocks = numDBlocks)))

  }
  unsigned8 = function(x) {
    if (x < 0)
      return(x + 256)
    else return(x)
  }
  readDataBlock = function(fid, complete = TRUE) {
    idstr = readChar(fid, 2, useBytes = TRUE)
    if (length(idstr) == 0 || idstr != "AX") {
      return(invisible(NULL))
    }
    else {
      readChar(fid, 2, useBytes = TRUE)
      tsOffset = readBin(fid, integer(), size = 2)
      readChar(fid, 8, useBytes = TRUE)
      timeStamp = readBin(fid, integer(), size = 4)
      light = 2^(3 * (readBin(fid, integer(), size = 2)/512 +
                        1))
      temperature = (150 * readBin(fid, integer(), size = 2) -
                       20500)/1000
      readChar(fid, 1, useBytes = TRUE)
      battery = 3 * (unsigned8(readBin(fid, integer(),
                                       size = 1))/512 + 1)
      frequency = readBin(fid, integer(), size = 1)
      temp = readBin(fid, integer(), size = 1)
      packed = bitwAnd(temp, 15) == 0
      temp = readBin(fid, integer(), size = 2)
      blockLength = readBin(fid, integer(), size = 2)
      shift = 0
      fractional = 0
      if (frequency != 0) {
        shift = temp
        if (bitwAnd(tsOffset, 32768L) != 0) {
          frequency = round(3200/bitwShiftL(1, 15 - bitwAnd(frequency,
                                                            15)))
          fractional = bitwShiftL(bitwAnd(tsOffset, 32767L),
                                  1)
          shift = shift + bitwShiftR((fractional * frequency),
                                     16)
        }
      }
      else {
        frequency = temp
      }
      if (complete) {
        if (packed) {
          packedData = readBin(fid, integer(), size = 4,
                               n = blockLength)
          data = GGIRread:::AxivityNumUnpack(packedData)
          temp = 482 - 4 * blockLength
        }
        else {
          xyz = readBin(fid, integer(), size = 2, n = blockLength *
                          3)
          data = matrix(xyz, ncol = 3, byrow = T)
          temp = 482 - 6 * blockLength
        }
        readChar(fid, temp, useBytes = TRUE)
        colnames(data) = c("x", "y", "z")
        data = data/256
      }
      else {
        readChar(fid, 482, useBytes = TRUE)
      }
      l = list(frequency = frequency, start = timestampDecoder(timeStamp,
                                                               fractional, -shift/frequency), temperature = temperature,
               battery = battery, light = light, length = blockLength)
      if (complete) {
        l$data = data
      }
      return(invisible(l))
    }
  }
  nargin = nargs()
  if (nargin < 1) {
    stop("At least file must be specified")
  }
  numDBlocks = round(file.info(file)$size/512) - 2
  pageLength = 300
  fid = file(file, "rb")
  struc = list(0, 0L)
  header = readHeader(fid, numDBlocks)
  origin = as.numeric(header$start)
  step = 1/header$frequency
  if (is.numeric(start)) {
    if (start < 0)
      start = 0
    start = origin + start * pageLength * step
  }
  if (is.numeric(end)) {
    end = end * pageLength
    if (end > numDBlocks * 150) {
      end = numDBlocks * 150
    }
    end = origin + end * step
  }
  if (end <= start) {
    close(fid)
    return(invisible(list(header = header, data = NULL)))
  }
  seek(fid, 0)
  readChar(fid, 1024, useBytes = TRUE)
  timeRes = seq(start, end, step)
  nr = length(timeRes) - 1
  timeRes = as.vector(timeRes[1:nr])
  accelRes = matrix(0, nrow = nr, ncol = 3, dimnames = list(NULL,
                                                            c("x", "y", "z")))
  temp = vector(mode = "double", nr)
  battery = vector(mode = "double", nr)
  light = vector(mode = "double", nr)
  if (progressBar)
    pb = txtProgressBar(1, nr, style = 3)
  pos = 1
  prevRaw = readDataBlock(fid)
  if (is.null(prevRaw)) {
    close(fid)
    return(invisible(list(header = header, data = NULL)))
  }
  rawTime = vector(mode = "numeric", 300)
  rawAccel = matrix(nrow = 300, ncol = 3)
  rawPos = 1
  for (i in 2:numDBlocks) {
    raw = readDataBlock(fid)
    if (is.null(raw))
      break
    prevStart = prevRaw$start
    prevLength = prevRaw$length
    if (raw$start < start) {
      prevRaw = raw
      next
    }
    time = seq(prevStart, raw$start, length.out = prevLength +
                 1)
    if (rawPos == 1) {
      rawAccel[1, ] = (prevRaw$data[1, ])
      rawTime[1] = prevStart - 1e-05
      rawPos = 2
    }
    rawLast = prevLength + rawPos - 1
    rawTime[rawPos:rawLast] = time[1:prevLength]
    rawAccel[rawPos:rawLast, ] = as.matrix(prevRaw$data)
    lastTime = time[prevLength]
    last = pos + 200
    if (pos + 200 > nr)
      last = nr
    tmp = GGIRread:::resample(rawAccel, rawTime, timeRes[pos:last],
                              rawLast)
    last = nrow(tmp) + pos - 1
    if (last >= pos) {
      accelRes[pos:last, ] = tmp
    }
    rawTime[1] = rawTime[rawLast]
    rawAccel[1, ] = rawAccel[rawLast, ]
    rawPos = 2
    if (last >= pos) {
      light[pos:last] = prevRaw$light
      temp[pos:last] = prevRaw$temperature
      battery[pos:last] = prevRaw$battery
    }
    prevRaw = raw
    pos = last + 1
    if (progressBar)
      setTxtProgressBar(pb, pos)
    if (pos > nr)
      break
  }
  if (pos <= nr) {
    newTimes = (prevRaw$start - prevStart)/prevLength * prevRaw$length +
      prevRaw$start
    prevLength = prevRaw$length
    time = seq(prevStart, newTimes, length.out = prevLength +
                 1)
    if (rawPos == 1) {
      rawAccel[1, ] = (prevRaw$data[1, ])
      rawTime[1] = prevStart - 1e-05
      rawPos = 2
    }
    rawLast = prevLength + rawPos - 1
    rawTime[rawPos:rawLast] = time[1:prevLength]
    rawAccel[rawPos:rawLast, ] = as.matrix(prevRaw$data)
    lastTime = time[prevLength]
    last = pos + 200
    if (pos + 200 > nr)
      last = nr
    tmp = GGIRread:::resample(rawAccel, rawTime, timeRes[pos:last],
                              rawLast)
    last = nrow(tmp) + pos - 1
    if (last >= pos) {
      accelRes[pos:last, ] = tmp
      light[pos:last] = prevRaw$light
      temp[pos:last] = prevRaw$temperature
      battery[pos:last] = prevRaw$battery
    }
  }
  close(fid)
  emptydata = which(rowSums(accelRes) == 0 & temp == 0 & battery ==
                      0 & light == 0)
  if (length(emptydata) > 0) {
    startends = which(diff(emptydata) != 1)
    if (length(startends) > 0) {
      lastmeasurement = max(startends)
    }
    else {
      lastmeasurement = emptydata[1]
    }
    if (length(lastmeasurement) > 0) {
      cut = c(lastmeasurement:nrow(accelRes))
      accelRes = accelRes[-cut, ]
      battery = battery[-cut]
      light = light[-cut]
      temp = temp[-cut]
      timeRes = timeRes[-cut]
    }
  }
  # return(invisible(list(header = header, data = as.data.frame(cbind(time = timeRes,
  #                                                                   accelRes, temp, battery, light)))))
  return(invisible(list(header = header, data = cbind(accelRes))))
}

