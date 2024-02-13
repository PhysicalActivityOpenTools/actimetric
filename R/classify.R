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
#' @param boutdur Numeric vector (default = c(1, 10, 30)) indicating the bout durations over which calculate bouts of behaviors
#' @param boutcriter Numeric (default = 0.8) indicating the proportion of the bout duration that should be classified in a given behavior to consider a bout
#'
#' @return Function does not return anything, it only generates the reports and
#' visualizations in the \code{output_directory}.
#' @export
#' @references van hees 2014; neishabouri 2022
#' @importFrom caret predict.train
#'
#' @author Jairo H. Migueles <jairo@jhmigueles.com>
classify = function(input_directory = NULL, output_directory = NULL, studyname = "actimetric",
                    do.calibration = TRUE, do.sleep = TRUE, do.nonwear = TRUE,
                    do.enmo = TRUE, do.actilifecounts = FALSE,
                    do.actilifecountsLFE = FALSE,
                    classifier = NULL, boutdur = c(10), boutcriter = 0.8,
                    overwrite = FALSE, verbose = TRUE) {
  # digits options
  options(digits.secs = 3)

  # Check directories and list files...
  if (dir.exists(input_directory)) files = dir(input_directory, recursive = TRUE, full.names = TRUE)
  if (!dir.exists(input_directory)) files = input_directory
  # create output folder
  output_directory = file.path(output_directory, paste0("output_", studyname))
  suppressWarnings({
    # dir.create(file.path(output_directory, "raw"), recursive = TRUE)
    dir.create(file.path(output_directory, "time_series"), recursive = TRUE)
    dir.create(file.path(output_directory, "summary"), recursive = TRUE)
    dir.create(file.path(output_directory, "results"), recursive = TRUE)
  })

  # Run pipeline ----------------------------------------------------------
  for (file in files) {
    # filename to save milestone of raw data
    fn2save = file.path(output_directory, "time_series", paste0(basename(file), ".RData"))
    if (overwrite == TRUE | file.exists(fn2save) == FALSE) {

      # 1 - read data
      raw = ReadAccFile(file, verbose = verbose)
      fileInfo = raw$header; start_time = raw$start_time; ID = raw$ID; sf = raw$sf
      raw = raw$data

      # 2 - calibrate
      calCoefs = vm.error.st = vm.error.end = NULL
      if (do.calibration == TRUE) {
        raw = calibrateRaw(raw, sf = sf, verbose = verbose)
        if (is.list(raw)) {
          calCoefs = raw$calCoefs; vm.error.st = raw$vm.error.st;
          vm.error.end = raw$vm.error.end
          raw = raw$raw
        }
      }
      # 3 - extract features
      # 3.1 - get info for classifier
      infoClassifier = GetInfoClassifier(classifier = classifier)
      epoch = infoClassifier$epoch; classes = infoClassifier$classes
      rfmodel = infoClassifier$rfmodel; hmmmodel = infoClassifier$hmmmodel
      # 3.2 - set loop to read data in chunks and classify
      prevChunk = 0; lastChunk = FALSE
      while (lastChunk == FALSE) {
        select = chunkIndexing(prevChunk = prevChunk, sf = sf, rawEnd = nrow(raw))
        # info for next iteration
        prevChunk = prevChunk + 1; lastChunk = select$lastChunk
        # indices to read in current iteration
        select = select$select
        # print progress in console
        if (verbose == TRUE) {
          from = round(min(select)/sf/3600, 2)
          to = round(max(select)/sf/3600, 2)
          total = round(nrow(raw)/sf/3600, 2)
          cat(paste("\rClassifying hours", from, "to", to, "out of", total, "\r"))
        }
        # 3.3 - detect non wear in 24h chunk
        if (do.nonwear) {
          if (nrow(raw[select,]) >= (sf*60*60)) { # at least 1hr of data
            nw = detectNonWear(raw[select,], sf = sf, epoch = epoch)
            hold = nw[length(nw)]
          } else { # repeat last value from previous chunk if available
            if (exists("hold")) {
              nw = rep(hold, nrow(raw[select, ]) / (epoch*sf))
            } else { # or set the full time as wear if full recording < 1 hour
              nw = rep(0, nrow(raw[select, ]) / (epoch*sf))
            }
          }
        } else {
          nw = rep(0, nrow(raw[select, ]))
        }
        nw = matrix(data = nw, nrow = length(nw), ncol = 1,
                    dimnames = list(1:length(nw), "nonwear"))
        # 3.4 - extract time series in 24h chunk
        feats = ExtractFeatures(raw[select,], classifier = classifier,
                                epoch = epoch, sf = sf,
                                do.enmo = do.enmo, do.actilifecounts = do.actilifecounts,
                                do.actilifecountsLFE = do.actilifecountsLFE,
                                ID = ID)
        feats = cbind(feats, nw)
        if (prevChunk == 1) ts = feats else ts = rbind(ts, feats)
      }
      # 3.5 - derive timestamp and ID
      timestamp = deriveTimestamps(from = start_time, length = nrow(ts), epoch = epoch)
      subject = rep(ID, nrow(ts))
      ts = as.data.frame(cbind(subject, timestamp, ts))
      numeric_columns = sapply(ts, mode) == 'numeric'
      ts[numeric_columns] =  round(ts[numeric_columns], 3)
      # 4 - apply classifier
      ts  = do.call(data.frame,lapply(ts, function(x) replace(x, is.infinite(x), NA)))
      ts[is.na(ts)] = 0
      ts$activity = caret::predict.train(rfmodel, ts)
      ts$activity = as.numeric(ts$activity)
      # 5 - detect sleep
      ts$sleep_windows_orig = ts$sleep_periods = ts$sleep = 0
      if (do.sleep) {
        ts = detectSleep(data = raw, ts = ts, epoch = 5, sf = sf, start_time = start_time)
      }
      # MILESTONE: save features data in features folder
      original_classifier = classifier
      save(ts, fileInfo, sf, start_time, ID, calCoefs, vm.error.st, vm.error.end,
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

}
