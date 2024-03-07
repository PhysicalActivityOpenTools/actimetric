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
runActimetric = function(input_directory = NULL, output_directory = NULL, studyname = "actimetric",
                         do.calibration = TRUE, do.sleep = TRUE, do.nonwear = TRUE,
                         do.enmo = TRUE, do.actilifecounts = FALSE,
                         do.actilifecountsLFE = FALSE,
                         classifier = NULL, boutdur = c(10), boutcriter = 0.8,
                         overwrite = FALSE, verbose = TRUE) {
  # Options
  options(digits.secs = 3)
  classifier = tolower(classifier)
  # -------------------------------------------------------------------------
  # Check directories and list files
  if (dir.exists(input_directory)) files = dir(input_directory, recursive = TRUE, full.names = TRUE)
  if (!dir.exists(input_directory)) files = input_directory
  output_directory = file.path(output_directory, paste0("output_", studyname))
  suppressWarnings({
    dir.create(file.path(output_directory, "time_series"), recursive = TRUE)
    dir.create(file.path(output_directory, "summary"), recursive = TRUE)
    dir.create(file.path(output_directory, "results"), recursive = TRUE)
  })
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # Load files...
  for (file in files) {
    fn2save = file.path(output_directory, "time_series",
                        paste0(basename(file), ".RData"))
    if (overwrite == TRUE | file.exists(fn2save) == FALSE) {
      # load and classify 24 hours of data -------
      if (verbose) {
        cat(paste("\nReading", file, "...\n"))
        cat(paste("File Size:", round(file.info(file)$size/1024^2, 1), "MB"))
      }
      # ---------------------------------------------------------------------
      # file information relevant to read file
      # file format and ID
      dot_position = regexpr("\\.([[:alnum:]]+)$", file)
      format = substr(file, dot_position + 1, nchar(file))
      ID = gsub(paste0(".", format, "$"), "", basename(file))
      infoClassifier = GetInfoClassifier(classifier)
      epoch = infoClassifier$epoch; classes = infoClassifier$classes
      rfmodel = infoClassifier$rfmodel; hmmmodel = infoClassifier$hmmmodel
      # Extract file information
      I = GGIR::g.inspectfile(file)
      hvars = GGIR::g.extractheadervars(I)
      sf = I$sf
      # Extract parameters for reading file in chunks
      readParams = GGIR::get_nw_clip_block_params(chunksize = 1, dynrange = 8,
                                                 monc = I$monc, dformat = I$dformc,
                                                 sf = sf, rmc.dynamic_range = NULL)
      blocksize = readParams$blocksize
      isLastBlock = FALSE
      blocknumber = 1; iteration = 1
      PreviousLastValue = c(0, 0, 1); PreviousLastTime = PreviousEndPage = NULL
      S = matrix(0,0,4) #dummy variable needed to cope with head-tailing succeeding blocks of data
      # ---------------------------------------------------------------------
      # Run Pipeline...
      while (isLastBlock == FALSE) {
        browser()
        # 1 - read and extract calibration coefficients
        accread = ReadAndCalibrate(file = file, sf = sf, blocksize = blocksize,
                                   blocknumber = blocknumber,
                                   inspectfileobject = I,
                                   PreviousEndPage = PreviousEndPage,
                                   PreviousLastValue = PreviousLastValue,
                                   PreviousLastTime = PreviousLastTime,
                                   do.calibration = do.calibration,
                                   iteration = iteration, epoch = epoch,
                                   isLastBlock = isLastBlock, S = S, verbose = verbose)
        data = accread$data; calCoefs = accread$calCoefs; vm.error.st = accread$vm.error.st
        vm.error.end = accread$vm.error.end; blocknumber = accread$blocknumber
        PreviousLastValue = accread$PreviousLastValue
        PreviousLastTime = accread$PreviousLastTime; PreviousEndPage = accread$PreviousEndPage
        isLastBlock = accread$isLastBlock; S = accread$S
        remaining_epochs = accread$remaining_epochs; nHoursRead = accread$nHoursRead
        if (iteration == 1) starttime = accread$starttime
        iteration = iteration + 1
        rm(accread); gc()
        # 2 - classify and save time series if required (only FALSE when using function within GGIR)
        if (!is.null(data)) {
          activity = classify(data = data, fn2save = fn2save,
                              do.calibration = do.calibration,
                              do.sleep = do.sleep, do.nonwear = do.nonwear,
                              do.enmo = do.enmo, do.actilifecounts = do.actilifecounts,
                              do.actilifecountsLFE = do.actilifecounts,
                              classifier = classifier)
        }
      }

      # 3 - store ts
      while (isLastBlock == FALSE) {}
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

    # 6 - aggregate per date
    if (do.sleep) classes = c(classes, "nighttime")
    if (do.nonwear) classes = c(classes, "nonwear")
    daysummary = aggregate_per_date(ts = ts, epoch = epoch, classifier = classifier,
                                    classes = classes, boutdur = boutdur, boutcriter = boutcriter)
    fn2save = file.path(output_directory, "summary", paste0(basename(file), ".RData"))
    save(daysummary, file = fn2save)
    # 7 - visualize (to be done)
  }
  # 8 - Generate reports
  files2summarize = dir(file.path(output_directory, "summary"), full.names = TRUE)
  doReport(files2summarize)

}
