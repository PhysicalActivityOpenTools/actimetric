#' Classifies Accelerometer Data into Physical Activity Types
#'
#' @description
#' This is the central function of the actimetric package.
#'
#' @param input_directory Character with full path to directory containing raw
#' accelerometer data. At the moment the package accepts ActiGraph gt3x,
#' axivity cwa, and GENEActiv bin files. The \code{input_directory} should only
#' contain accelerometer files, they can be organized in subdirectories within
#' the input directory if needed.
#' @param output_directory Character with full path to directory where the reports
#' and visualizations are to be stored.
#' @param do.calibration Logical (default = TRUE) indicating whether the accelerometer
#' raw signal should be calibrated relative to local gravitational acceleration.
#' @param do.sleep Logical (default = TRUE) indicating whether sleep parameters
#' should be calculated.
#' @param do.nonwear Logical (default = TRUE) indicating whether non-wear time
#' should be derived.
#' @param classifier Character (default = NULL) indicating the classifier to be used
#' (available options are:
#' Preschool Wrist Random Forest Free Living,
#' Preschool Hip Random Forest Free Living,
#' Preschool Hip Random Forest Free Living Lag-Lead,
#' Preschool Wrist Random Forest Free Living Lag-Lead,
#' School age Wrist Random Forest, School age Hip Random Forest,
#' Adult Wrist RF Trost,
#' Adult women Wrist RF Ellis,
#' Adult women Hip RF Ellis,
#' Thigh Decision Tree)
#' @param studyname Character (default = "actimetric") defining the name of the study.
#' To be used to give a name to the output directory generated.
#' @param verbose Logical (default = TRUE) indicating whether progress messages should
#' be printed in the console.
#' @param do.enmo Logical (default = TRUE) indicating whether ENMO should be
#' calculated.
#' @param do.actilifecounts Logical (default = FALSE) indicating whether activity
#' counts should be calculated.
#' @param do.actilifecountsLFE Logical (default = FALSE) indicating whether activity
#' counts using the low-frequency extension filter should be calculated.
#' @param overwrite Logical (default = FALSE) indicating whether the existing milestone
#' data should be overwritten.
#' @param boutdur Numeric vector (default = c(10)) indicating the bout durations over which calculate bouts of behaviors
#' @param boutcriter Numeric (default = 0.8) indicating the proportion of the bout duration that should be classified in a given behavior to consider a bout
#'
#' @return Function does not return anything, it only generates the reports and
#' visualizations in the \code{output_directory}.
#' @export
#' @references van hees 2014; neishabouri 2022
#' @importFrom caret predict.train
#' @importFrom stats predict
#' @import randomForest
#' @importFrom HMM viterbi
#'
#' @author Jairo H. Migueles <jairo@jhmigueles.com>
classify = function(data = NULL, fn2save = NULL,
                    do.calibration = TRUE, do.sleep = TRUE, do.nonwear = TRUE,
                    do.enmo = TRUE, do.actilifecounts = FALSE,
                    do.actilifecountsLFE = FALSE,
                    classifier = NULL, save_timeSeries = TRUE) {
  fn2save = file.path(output_directory, "time_series", paste0(basename(file), ".RData"))
  if (overwrite == TRUE | file.exists(fn2save) == FALSE) {
    # load and classify 24 hours of data -------
    if (verbose) {
      cat(paste("\nReading", file, "...\n"))
      cat(paste("File Size:", round(file.info(file)$size/1024^2, 1), "MB"))
    }
    # extract ID and file extension (format) ------------
    dot_position = regexpr("\\.([[:alnum:]]+)$", file)
    format = substr(file, dot_position + 1, nchar(file))
    ID = gsub(paste0(".", format, "$"), "", basename(file))
    # file information relevant to read file
    I = GGIR::g.inspectfile(file)
    sf = I$sf
    blocksize = getBlocksizeReadaccfile(file, sf, I$monc, I$dformc)
    isLastBlock = FALSE
    blocknumber = 1; iteration = 1
    PreviousLastValue = c(0, 0, 1); PreviousLastTime = NULL; PreviousEndPage = NULL
    while (isLastBlock == FALSE) {
      # 1 - read data
      accread = GGIR::g.readaccfile(filename = file,
                                    blocksize = blocksize,
                                    blocknumber = blocknumber,
                                    filequality = data.frame(filetooshort = FALSE,
                                                             filecorrupt = FALSE,
                                                             filedoesnotholdday = FALSE),
                                    ws = 3600,
                                    PreviousEndPage = PreviousEndPage, inspectfileobject = I,
                                    PreviousLastValue = PreviousLastValue,
                                    PreviousLastTime = PreviousLastTime)
      raw = accread$P$data # first column contains timestamp
      if (blocknumber == 1) start_time = accread$P$data[1,1]
      # information for next iteration
      blocknumber = blocknumber + 1; isLastBlock = accread$isLastBlock
      PreviousEndPage = accread$endpage
      if ("PreviousLastValue" %in% names(accread$P)) { # output when reading ad-hoc csv
        PreviousLastValue = accread$P$PreviousLastValue
        PreviousLastTime = accread$P$PreviousLastTime
      }
      # If calibration is required, then first load 72 hours to get cal coefs
      # hours read
      len = nrow(accread$P$data)
      from = strptime(as.POSIXct(raw[1,1], origin = "1970-1-1"),
                      format = "%Y-%m-%d %H:%M:%S")
      to = strptime(as.POSIXct(raw[len,1], origin = "1970-1-1"),
                    format = "%Y-%m-%d %H:%M:%S")
      if (verbose) {
        cat("Loading data from", as.character(from), "to", as.character(to), "\r")
      }
      nHoursRead = difftime(to, from, units = "hours")
      # if calibration is required, we need to load extra time (up to 72 hours)
      if (do.calibration & iteration == 1) {
        while (nHoursRead < 72 & isLastBlock == FALSE) {
          accread = GGIR::g.readaccfile(filename = file,
                                        blocksize = blocksize,
                                        blocknumber = blocknumber,
                                        filequality = data.frame(filetooshort = FALSE,
                                                                 filecorrupt = FALSE,
                                                                 filedoesnotholdday = FALSE),
                                        ws = 3600,
                                        PreviousEndPage = PreviousEndPage, inspectfileobject = I,
                                        PreviousLastValue = PreviousLastValue,
                                        PreviousLastTime = PreviousLastTime)
          raw = rbind(raw, accread$P$data) # first column contains timestamp
          # information for next iteration
          blocknumber = blocknumber + 1; isLastBlock = accread$isLastBlock
          PreviousEndPage = accread$endpage
          if ("PreviousLastValue" %in% names(accread$P)) { # output when reading ad-hoc csv
            PreviousLastValue = accread$P$PreviousLastValue
            PreviousLastTime = accread$P$PreviousLastTime
          }
          # hours read
          len = nrow(raw)
          from = strptime(as.POSIXct(start_time, origin = "1970-1-1"),
                          format = "%Y-%m-%d %H:%M:%S")
          to = strptime(as.POSIXct(raw[len,1], origin = "1970-1-1"),
                        format = "%Y-%m-%d %H:%M:%S")
          if (verbose) {
            cat("\nLoading data from", as.character(from), "to", as.character(to))
          }
          nHoursRead = difftime(to, from, units = "hours")
        }
        # 2 - get calibration coefficients
        calCoefs = vm.error.st = vm.error.end = NULL
        if (do.calibration == TRUE & iteration == 1) {
          cal = calibrateRaw(raw, sf = sf, verbose = verbose)
          if (is.list(cal)) {
            calCoefs = cal$calCoefs; vm.error.st = cal$vm.error.st;
            vm.error.end = cal$vm.error.end
          }
        }
      }
      if (!is.null(raw)) {
        # 2 - calibrate
        if (do.calibration == TRUE) {
          raw[, 1] = calCoefs$scale[1]*(raw[,1] - calCoefs$offset[1])
          raw[, 2] = calCoefs$scale[2]*(raw[,2] - calCoefs$offset[2])
          raw[, 3] = calCoefs$scale[3]*(raw[,3] - calCoefs$offset[3])
        }
        # 3 - extract features
        # 3.1 - get info for classifier
        infoClassifier = GetInfoClassifier(classifier = classifier)
        epoch = infoClassifier$epoch; classes = infoClassifier$classes
        rfmodel = infoClassifier$rfmodel; hmmmodel = infoClassifier$hmmmodel
        body_attachment_site = NA
        if (grepl("hip", classifier, ignore.case = TRUE)) {
          body_attachment_site = "hip"
        } else if (grepl("wrist", classifier, ignore.case = TRUE)) {
          body_attachment_site = "wrist"
        } else if (grepl("thigh", classifier, ignore.case = TRUE)) {
          body_attachment_site = "thigh"
        }
        # 3.2 - Classify current chunk of raw data
        # 3.3 - detect non wear in 24h chunk
        if (do.nonwear) {
          if (nrow(raw) >= (sf*60*60)) { # at least 1hr of data
            nw = detectNonWear(raw, sf = sf, epoch = epoch)
            hold = nw[length(nw)]
          } else { # repeat last value from previous chunk if available
            if (exists("hold")) {
              nw = rep(hold, nrow(raw) / (epoch*sf))
            } else { # or set the full time as wear if full recording < 1 hour
              nw = rep(0, nrow(raw) / (epoch*sf))
            }
          }
        } else {
          nw = rep(0, nrow(raw))
        }
        nw = matrix(data = nw, nrow = length(nw), ncol = 1,
                    dimnames = list(1:length(nw), "nonwear"))
        # 3.4 - extract time series in 24h chunk
        feats = ExtractFeatures(raw[,-1], classifier = classifier,
                                epoch = epoch, sf = sf,
                                do.enmo = do.enmo, do.actilifecounts = do.actilifecounts,
                                do.actilifecountsLFE = do.actilifecountsLFE,
                                ID = ID)
        feats$features = cbind(feats$features, nw)
        if (iteration == 1) ts = feats$features else ts = rbind(ts, feats$features)
        if (iteration == 1) anglez = feats$anglez else anglez = rbind(anglez, feats$anglez)
        # remove raw data chunk to free memory
        iteration = iteration + 1
        rm(accread); gc()
      }
    }
    # Now add lag-lead features if needed
    if (grepl("lag-lead", classifier, ignore.case = TRUE)) {
      lagsd1 = c(0, ts$vm.sd[1:c(nrow(ts) - 1)])
      lagsd2 = c(0, 0, ts$vm.sd[1:c(nrow(ts) - 2)])
      leadsd1 = c(ts$vm.sd[2:nrow(ts)], 0)
      leadsd2 = c(ts$vm.sd[3:nrow(ts)], 0, 0)
      combsd = apply(cbind(lagsd1, lagsd2, leadsd1, leadsd2), 1, sd)
      laglead = cbind(lagsd1, lagsd2, leadsd1, leadsd2, combsd)
      ts = cbind(ts, laglead)
    }
    # 3.5 - derive timestamp and ID
    timestamp = deriveTimestamps(from = start_time, length = nrow(ts), epoch = epoch)
    subject = rep(ID, nrow(ts))
    ts = as.data.frame(cbind(subject, timestamp, ts))
    numeric_columns = sapply(ts, mode) == 'numeric'
    ts[numeric_columns] =  round(ts[numeric_columns], 3)
    ts_anglez = deriveTimestamps(from = start_time, length = length(anglez), epoch = 5)
    anglez = data.frame(date = ts_anglez[, 1], time = ts_anglez[, 2], anglez = anglez)
    # 4 - apply classifier
    ts  = do.call(data.frame,lapply(ts, function(x) replace(x, is.infinite(x), NA)))
    ts[is.na(ts)] = 0
    if (grepl("thigh", classifier, ignore.case = TRUE)) {
      ts$activity = ts$post = thres = NA
      sit = which(ts$bfsd.x <= 0.1 & ts$inc.x < 135)
      ts$activity[sit] = 1
      e  = apply(ts[, c("bfsd.y", "bfsd.x", "bfsd.z")], 1, max)
      standMove = which(e > 0.1 & ts$bfsd.x <= 0.1 & abs(ts$inc.x) >= 135)
      ts$activity[standMove] = 3
      stand = which(e <= 0.1 & ts$bfsd.x <= 0.1 & ts$inc.x >= 135)
      ts$activity[stand] = 2
      ee = which(is.na(ts$activity))
      bike = which(ts$bfsd.x[ee] > 0.1 & ts$fb.z[ee] > 24)
      ts$activity[ee[bike]] = 7
      run = which(ts$bfsd.x[ee] > 0.1 & ts$bfsd.x[ee] > 0.72 & ts$fb.z[ee] < 24)
      ts$activity[ee[run]] = 5
      walk = which(ts$bfsd.x[ee] > 0.1 & ts$bfsd.x[ee] < 0.72 & ts$fb.z[ee] < 24)
      ts$activity[ee[walk]] = 4
      e = which(ts$activity == 1)
      if (length(e) > 0) {
        thres = as.numeric(abs(base::summary(ts$fb.z[e & ts$fb.z[e] < 5 & ts$fb.z[e] > -5])[3]) + 4.5)
        climbStairs = which(ts$activity %in% c(4:5) & ts$fb.z > thres)
        ts$activity[climbStairs] = 6
      }
      if (length(e) == 0 & !is.na(thres)) {
        climbStairs = which(ts$activity %in% c(4:5) & ts$fb.z > thres)
        ts$activity[climbStairs] = 6
      }
    } else if (is.null(hmmmodel)) {
      ts$activity = tryCatch(stats::predict(rfmodel, ts),
                             error = function(e) caret::predict.train(rfmodel, ts))
    } else {
      # For now, only Ellis classifiers
      testDat = ts[, which(colnames(ts) == "mean"):which(colnames(ts) == "fft14")]
      testDat = predict(rfmodel$preProcValues, testDat)
      activity = stats::predict(rfmodel,testDat)
      ts[, 4:44] = testDat
      filtered = HMM::viterbi(hmmmodel, activity)
      ts = cbind(ts, activity, filtered)
      ts$activity_orig = ts$activity
      ts$hmm_orig = ts$filtered
    }
    ts$activity = as.numeric(ts$activity)
    # 5 - detect sleep
    ts$sleep_windows_orig = ts$sleep_periods = ts$sleep = 0
    if (do.sleep) {
      sleep_id = length(classes) + 1
      nonwear_id = length(classes) + 2
      if (body_attachment_site == "wrist") {
        ts = detectSleep(ts = ts, anglez = anglez, epoch = 5,
                         sleep_id = sleep_id, nonwear_id = nonwear_id)
      } else {
        warning("Sleep detection only available for wrist and thigh attachment sites.")
      }

    }
    # MILESTONE: save features data in features folder
    original_classifier = classifier
    save(ts, sf, start_time, ID, calCoefs, vm.error.st, vm.error.end,
         original_classifier, file = fn2save)
  } else { # if already existed and not overwritting...
    if (verbose) cat(paste("\nLoading classified time series...\n"))
    load(fn2save)
    # reload info for classifier
    if (classifier != original_classifier) stop("Classifier changed from the previous run, please run with overwrite = TRUE.")
    infoClassifier = GetInfoClassifier(classifier = classifier)
    epoch = infoClassifier$epoch; classes = infoClassifier$classes
    rfmodel = infoClassifier$rfmodel; hmmmodel = infoClassifier$hmmmodel
  }


}

