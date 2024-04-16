#' Read Accelerometer Raw Data File and Extract Calibration Coefficients
#'
#' @param blocksize Size of chunk to be read in pages as extracted from \code{GGIR::get_nw_clip_block_params}.
#' @param blocknumber Number of chunk of data to be read.
#' @param PreviousEndPage Last page number read in the previous iteration.
#' @param inspectfileobject Inspect file object extracted from \code{GGIR::g.inspectfile}.
#' @param PreviousLastValue Last acceleration value(s) read in the previous iteration.
#' @param PreviousLastTime Last timestamp read in the previous iteration.
#' @param file Path to the file to be read.
#' @param verbose Logical indicating whether to print progress messages in the console.
#' @param do.calibration Logical indicating whether to calibrate the data or not.
#' @param iteration Number indicating the iteration in the \code{runActimetric} function.
#' @param sf Number with the sampling frequency in the recording.
#' @param epoch Number with the desired epoch length for the aggregation in seconds.
#' @param isLastBlock Logical indicating if this is the last chunk of data to be read in the file.
#' @param S Leftover data from the previous iteration, to be appended to the current chunk of data being read.
#'
#' @description
#' Function aimed to read accelerometer raw data. At the moment,
#' the function supports ActiGraph gt3x and csv data, GENEActiv bin
#' and csv data
#'
#' @return List containing header info, accelerometer raw data, start time and ID
#' for the file.
#' @export
#' @import GGIR
#' @import GGIRread
ReadAndCalibrate = function(file, sf, blocksize, blocknumber, inspectfileobject,
                            PreviousEndPage, PreviousLastValue, PreviousLastTime,
                            isLastBlock, do.calibration, iteration, epoch, S,
                            verbose) {
  remaining_epochs = NULL
  # -------------------------------------------------------------------------
  # MODULE 1 - READ CHUNK OF DATA -------------------------------------------
  filequality = data.frame(filetooshort = FALSE, filecorrupt = FALSE, filedoesnotholdday = FALSE)
  minHours2load = ifelse(do.calibration == TRUE & iteration == 1, 72, 0)
  nHoursRead = 0
  count = 0
  while (nHoursRead <= minHours2load & isLastBlock == FALSE) {
    accread = GGIR::g.readaccfile(filename = file, blocksize = blocksize, blocknumber = blocknumber,
                                  filequality = filequality, ws = 3600,
                                  PreviousEndPage = PreviousEndPage,
                                  inspectfileobject = inspectfileobject,
                                  PreviousLastValue = PreviousLastValue,
                                  PreviousLastTime = PreviousLastTime)
    # information for next iteration
    blocknumber = blocknumber + 1; count = count + 1
    isLastBlock = accread$isLastBlock
    PreviousEndPage = accread$endpage
    if ("PreviousLastValue" %in% names(accread$P)) { # output when reading ad-hoc csv
      PreviousLastValue = accread$P$PreviousLastValue
      PreviousLastTime = accread$P$PreviousLastTime
    }
    if (length(accread$P) > 0) {
      if (count == 1) data = accread$P$data else data = rbind(data, accread$P$data)
      nHoursRead = nrow(data) / sf / 3600
    } else {
      data = NULL
    }
    rm(accread); gc()
  }

  # -------------------------------------------------------------------------
  # MODULE 2 - IMPUTE AND FORMAT CHUNK OF DATA ------------------------------
  if (!is.null(data)) { # would be NULL if not sufficient data
    # 2 - Impute time gaps if format is gt3x or csv
    if (inspectfileobject$dformc == 2 ||
        inspectfileobject$dformc == 6) {
      P = GGIR::g.imputeTimegaps(data, sf = sf, k = 0.25,
                                 PreviousLastValue = PreviousLastValue,
                                 PreviousLastTime = PreviousLastTime,
                                 epochsize = c(epoch, 60))
      data = P$x
      PreviousLastValue = data[nrow(data), c("x", "y", "z")]
      if ("time" %in% colnames(data)) {
        PreviousLastTime = as.POSIXct(data$time[nrow(data)])
      } else {
        PreviousLastTime = NULL
      }
      rm(P); gc()
    }
    # -------------------------------------------------------------------------
    # MODULE 3 - EXTRACT CALIBRATION COEFFICIENTS -----------------------------
    calCoefs = vm.error.st = vm.error.end = NULL
    if (do.calibration == TRUE & iteration == 1) {
      cal = calibrateRaw(data, sf = sf, verbose = verbose)
      if (is.list(cal)) {
        calCoefs = cal$calCoefs; vm.error.st = cal$vm.error.st;
        vm.error.end = cal$vm.error.end
      }
    }
    # -------------------------------------------------------------------------
    # 3 - Get start time if this is the first iteration
    starttime = NULL
    if (iteration == 1) {
      starttime = GGIR::g.getstarttime(datafile = file, data = data,
                                       mon = inspectfileobject$monc,
                                       dformat = inspectfileobject$dformc,
                                       desiredtz = "",
                                       configtz = NULL)
      trunc_start = !starttime$sec %in% seq(0, 60, by = epoch)
      if (trunc_start == TRUE) {
        next_epoch = seq(0, 60, by = epoch)[seq(0, 60, by = epoch) > starttime$sec][1]
        lookhere = round((next_epoch - starttime$sec)*sf)
        if (any(grepl("time", colnames(data)))) {
          tcol = which(grepl("time", colnames(data)))
          lookhere = (lookhere - 4):(lookhere + 4)
          starthere = which.min(abs(next_epoch - as.POSIXlt(data[lookhere, tcol])$sec))
          start = lookhere[starthere]
          del = 1:(start - 1)
          data = data[-del,]
        } else {
          del = 1:lookhere
          data = data[-del,]
        }
        starttime = starttime + ((next_epoch - starttime$sec))
      }
      starttime = as.numeric(starttime)
    } else {
      starttime = data[1, "time"]
    }
    # 4 - Add leftover data hold from previous loop
    data = as.matrix(data, rownames.force = FALSE)
    if (nrow(S) > 0) {
      if ("remaining_epochs" %in% colnames(data)) {
        if (ncol(S) == (ncol(data) - 1)) {
          # this block has time gaps while the previous block did not
          S = cbind(S, 1)
          colnames(S)[4] = "remaining_epochs"
        }
      } else if ("remaining_epochs" %in% colnames(S)) {
        if ((ncol(S) - 1) == ncol(data)) {
          # this block does not have time gaps while the previous blog did
          data = cbind(data, 1)
          colnames(data)[4] = "remaining_epochs"
        }
      }
      data = rbind(S,data)
    }
    # 4 - Store data that  will be added to next block
    LD = nrow(data)
    if (LD >= (3600*sf)) { # if there is more than 1 hour of data...
      use = (floor(LD / (60*sf))) * (60*sf)
      if ((LD - use) > 1) {
        S = data[(use + 1):LD,] #store leftover data
        if (ncol(S) == 1) {
          S = t(S)
        }
      } else { # use all data
        S = matrix(0, 0, ncol(data))
      }
      data = data[1:use,]
      LD = nrow(data) # redefine LD because there is less data
      if ("remaining_epochs" %in% colnames(data)) { #
        # remove remaining_epochs from data object and keep it separately
        remaining_epochs = data[,"remaining_epochs"]
        data = data[, -which(colnames(data) == "remaining_epochs")]
      }
    }

    endtime = data[nrow(data), "time"]
    data = data[, c("x", "y", "z")]
    nHoursRead = nrow(data)/sf/3600
    # -------------------------------------------------------------------------
    return(list(data = data, calCoefs = calCoefs,
                vm.error.st = vm.error.st, vm.error.end = vm.error.end,
                blocknumber = blocknumber, PreviousLastValue = PreviousLastValue,
                PreviousLastTime = PreviousLastTime, PreviousEndPage = PreviousEndPage,
                isLastBlock = isLastBlock, S = S, remaining_epochs = remaining_epochs,
                starttime = starttime, endtime = endtime, nHoursRead = nHoursRead))
  } else { # no data to be read
    return(list(data = NULL, calCoefs = NULL,
                vm.error.st = NULL, vm.error.end = NULL,
                blocknumber = NULL, PreviousLastValue = NULL,
                PreviousLastTime = NULL, PreviousEndPage = NULL,
                isLastBlock = TRUE, S = NULL, remaining_epochs = NULL,
                starttime = NULL, endtime = NULL, nHoursRead = NULL))
  }
}
