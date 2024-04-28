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
#' Adult women Hip RF Ellis)
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
#' @param visualreport Logical (default = FALSE) indicating whether visualizations should be run and stored in the results folder.
#' @param n_valid_hours Numeric (default = 0) with minimum number of absolute valid hours in the day to consider it a valid day for the person-level aggregates.
#' @param n_valid_hours_awake Numeric (default = 0) with minimum number of absolute valid awake hours in the day to consider it a valid day for the person-level aggregates.
#' @param n_valid_hours_nighttime Numeric (default = 0) with minimum number of absolute valid nighttime hours in the day to consider it a valid day for the person-level aggregates.
#' @param boutmaxgap Integer (default = 1) with maximum consecutive gap length allowed in bout calculation.
#'
#' @return Function does not return anything, it only generates the reports and
#' visualizations in the \code{output_directory}.
#' @export
#' @references GGIR R Package. doi: 10.1123/jmpb.2018-0063
#' @importFrom caret predict.train
#' @importFrom stats predict
#' @importFrom HMM viterbi
#' @importFrom utils installed.packages packageVersion
#' @importFrom data.table fwrite
#' @import actilifecounts
runActimetric = function(input_directory = NULL, output_directory = NULL, studyname = "actimetric",
                         do.calibration = TRUE, do.sleep = TRUE, do.nonwear = TRUE,
                         do.enmo = TRUE, do.actilifecounts = FALSE,
                         do.actilifecountsLFE = FALSE,
                         classifier = NULL,
                         boutdur = c(10), boutcriter = 0.8, boutmaxgap = 1,
                         n_valid_hours = 0,
                         n_valid_hours_awake = 0, n_valid_hours_nighttime = 0,
                         visualreport = FALSE,
                         overwrite = FALSE, verbose = TRUE) {
  # get input
  LS = mget(ls())
  # Options
  options(digits.secs = 3)
  classifier = check_classifier(classifier)
  if (is.null(classifier)) {
    stop(paste0("The defined classifier is not within our current alternatives or probably it is mispelled.\n\n",
                " Please copy/paste one of the following clasifiers:\n",
                "    - Preschool Wrist Random Forest Free Living\n",
                "    - Preschool Hip Random Forest Free Living\n",
                "    - Preschool Hip Random Forest Free Living Lag-Lead\n",
                "    - Preschool Wrist Random Forest Free Living Lag-Lead\n",
                "    - School age Wrist Random Forest\n",
                "    - School age Hip Random Forest\n",
                "    - Adult Wrist RF Trost\n",
                "    - Adult women Wrist RF Ellis\n",
                "    - Adult women Hip RF Ellis\n"))
  }
  # info for classifier
  infoClassifier = GetInfoClassifier(classifier)
  classes = infoClassifier$classes
  suppressWarnings(rm("preschooltest", "preschool_hipfl_15s", "PS.RF.FL.Hip15.LagLead",
                      "PS.RF.FL.Wrist15.LagLead", "LVAY.RF.Wrist5.1", "LVAY.RF.Hip10",
                      "trostRF_7112014", "Ellis.Hip.RF.HMM", "Ellis.Wrist.RF.HMM"))
  epoch = infoClassifier$epoch; classes = infoClassifier$classes
  # body attachment site
  body_attachment_site = NULL
  if (grepl("wrist", classifier)) body_attachment_site = "wrist"
  if (grepl("hip|waist", classifier)) body_attachment_site = "hip"
  if (grepl("thigh", classifier)) body_attachment_site = "thigh"
  # check on do.sleep and body attachment site
  if (do.sleep == TRUE & body_attachment_site != "wrist") {
    warning("Sleep detection only available for wrist attachment site for now.\n
            Setting do.sleep to FALSE and n_valid_hours_nighttime = 0")
    do.sleep = FALSE
    n_valid_hours_nighttime = 0
  }
  # redefine classes
  if (do.sleep == TRUE) {
    classes = c(classes, "nighttime", "sleep")
  }
  if (do.nonwear == TRUE) {
    classes = c(classes, "nonwear")
  }
  # Welcome message
  if (verbose) {
    cat('\n')
    cat(paste0(rep('_', options()$width), collapse = ''))
    cat(paste0("\n\nWelcome to the actimetric R package!\n",
               "\nIf you experiment any technical issue or wish to contribute to actimetric,",
               "\nplease contact Jairo at jairo@jhmigueles.com or Matthew at matthew.ahmadi@sydney.edu.au\n\n"))
    cat(paste0(rep('_', options()$width), collapse = ''))
    cat(paste0("\n\nClassifier: ", classifier, "\n\n"))
    cat(paste0(rep('_', options()$width), collapse = ''))
    cat("\n")
  }
  # -------------------------------------------------------------------------
  # Check directories and list files
  if (dir.exists(input_directory)) files = dir(input_directory, recursive = TRUE, full.names = TRUE)
  if (!dir.exists(input_directory)) files = input_directory
  files = grep("gt3x$|cwa$|bin$", files, value = TRUE)
  output_directory = file.path(output_directory, paste0("output_", studyname))
  suppressWarnings({
    dir.create(file.path(output_directory, "time_series"), recursive = TRUE)
    dir.create(file.path(output_directory, "results"), recursive = TRUE)
  })
  # -------------------------------------------------------------------------
  # store configuration
  installedPackages = utils::installed.packages()[,1]
  GGIRversion = GGIRreadversion = actilifecountsversion = "not used"
  actimetricversion = as.character(utils::packageVersion("actimetric"))
  actimetricModelsversion = as.character(utils::packageVersion("actimetricModels"))
  if ("GGIR" %in% installedPackages) {
    GGIRversion = as.character(utils::packageVersion("GGIR"))
  }
  if ("GGIRread" %in% installedPackages) {
    GGIRreadversion = as.character(utils::packageVersion("GGIRread"))
  }
  if ("actilifecounts" %in% installedPackages & (do.actilifecounts == T | do.actilifecountsLFE == T)) {
    actilifecountsversion = as.character(utils::packageVersion("actilifecounts"))
  }
  Rversion = R.Version()$version.string
  OS = paste(Sys.info()[c("sysname", "release")], collapse = " ")
  analyses_starttime = format(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  LSnew = mget(ls())
  LSnew = LSnew[which(names(LSnew) %in% names(LS))]
  LSnew = t(list2DF(LSnew))
  config = data.frame(configuration = c("analyses_start_time", "OS", "Rversion",
                                        "actimetric_version",
                                        "actimetricModels_version",
                                        "GGIR_version", "GGIRread_version",
                                        "actilifecountsversion",
                                        rownames(LSnew)),
                      setting = c(analyses_starttime, OS, Rversion,
                                  actimetricversion, actimetricModelsversion,
                                  GGIRversion, GGIRreadversion,
                                  actilifecountsversion,
                                  LSnew[,1]))
  data.table::fwrite(config, file = file.path(output_directory, "configuration.csv"),
                     na = "", row.names = FALSE)
  # -------------------------------------------------------------------------
  # Load files...
  for (file in files) {
    # file format and ID
    dot_position = regexpr("\\.([[:alnum:]]+)$", file)
    format = substr(file, dot_position + 1, nchar(file))
    ID = gsub(paste0(".", format, "$"), "", basename(file))
    fn2save = file.path(output_directory, "time_series",
                        paste0(ID, ".RData"))
    if (overwrite == TRUE | file.exists(fn2save) == FALSE) {
      # load and classify 24 hours of data -------
      if (verbose) {
        cat(paste("\nReading", file, "...\n"))
        cat(paste("File Size:", round(file.info(file)$size/1024^2, 1), "MB"))
        cat("\n\n")
      }
      # ---------------------------------------------------------------------
      # file information relevant to read file
      I = GGIR::g.inspectfile(file)
      hvars = GGIR::g.extractheadervars(I)
      sf = I$sf
      # Extract parameters for reading file in chunks as in GGIR R package
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
      nonwear = enmo = agcounts = LFEcounts = tilt = anglez = activity = NULL
      while (isLastBlock == FALSE) {
        # 1 - read and extract calibration coefficients
        accread = ReadAndCalibrate(file = file, sf = sf, blocksize = blocksize,
                                   blocknumber = blocknumber,
                                   inspectfileobject = I,
                                   PreviousEndPage = PreviousEndPage,
                                   PreviousLastValue = PreviousLastValue,
                                   PreviousLastTime = PreviousLastTime,
                                   do.calibration = do.calibration,
                                   iteration = iteration, epoch = epoch,
                                   isLastBlock = isLastBlock, S = S, verbose = FALSE)
        starttime = accread$starttime; endtime = accread$endtime
        data = accread$data; blocknumber = accread$blocknumber
        PreviousLastValue = accread$PreviousLastValue
        PreviousLastTime = accread$PreviousLastTime; PreviousEndPage = accread$PreviousEndPage
        isLastBlock = accread$isLastBlock; S = accread$S
        remaining_epochs = accread$remaining_epochs; nHoursRead = accread$nHoursRead
        if (iteration == 1) {
          calCoefs = accread$calCoefs; vm.error.st = accread$vm.error.st
          vm.error.end = accread$vm.error.end
          recording_starttime = starttime
        }
        rm(accread); gc()
        # 2 - classify and save time series if required (only FALSE when using function within GGIR)
        if (!is.null(data)) {
          if (verbose) {
            t0 = format(as.POSIXct(starttime, origin = "1970-1-1"), "%Y-%m-%d %H:%M:%S")
            t1 = format(as.POSIXct(endtime, origin = "1970-1-1"), "%Y-%m-%d %H:%M:%S")
            cat(paste0("Processing data from ", t0, " to ", t1, "\r"))
          }
          # CALIBRATE CHUNK OF DATA
          if (do.calibration == TRUE) {
            if (is.list(calCoefs)) {
              data[, 1] = calCoefs$scale[1]*(data[,1] - calCoefs$offset[1])
              data[, 2] = calCoefs$scale[2]*(data[,2] - calCoefs$offset[2])
              data[, 3] = calCoefs$scale[3]*(data[,3] - calCoefs$offset[3])
            }
          }
          # Basic features ----------------------------------------------------------
          # non-wear
          nw = rep(0, nrow(data))
          if (do.nonwear) {
            if (nrow(data) >= (sf*60*60)) { # at least 1hr of data
              nw = detectNonWear(data, sf = sf, epoch = epoch)
              hold = nw[length(nw)]
            } else { # repeat last value from previous chunk if available
              if (exists("hold")) {
                nw = rep(hold, nrow(data) / (epoch*sf))
              } else { # or set the full time as wear if full recording < 1 hour
                nw = rep(0, nrow(data) / (epoch*sf))
              }
            }
          }
          nonwear = c(nonwear, nw)
          # enmo per epoch
          vm = sqrt(rowSums(data[, 1:3]^2))
          enm = NULL
          if (do.enmo) {
            enm = vm - 1
            enm[which(enm < 0)] = 0
            enm = slide(enm, width = epoch*sf, FUN = mean)
            enmo = c(enmo, enm)
          }
          # activity counts per epoch with default filter
          AGc = NULL
          if (do.actilifecounts) {
            AGc = actilifecounts::get_counts(raw = data[, 1:3], sf = sf, epoch = epoch,
                                             lfe_select = FALSE, verbose = FALSE)
            agcounts = rbind(agcounts, AGc)
            colnames(agcounts) = c("agcounts_x", "agcounts_y", "agcounts_z", "agcounts_vm")
          }
          # activity counts per epoch with LFE
          LFEc = NULL
          if (do.actilifecountsLFE) {
            LFEc = actilifecounts::get_counts(raw = data[, 1:3], sf = sf, epoch = epoch,
                                              lfe_select = TRUE, verbose = FALSE)
            LFEcounts = rbind(LFEcounts, LFEc)
            colnames(LFEcounts) = c("LFEcounts_x", "LFEcounts_y", "LFEcounts_z", "LFEcounts_vm")
          }
          # tilt
          tlt = NULL
          tlt = acos(data[,2]/vm)*(180/pi)
          tlt = slide(tlt, width = epoch*sf, FUN = mean)
          tilt = c(tilt, tlt)
          # z angle variability per 5 seconds
          if (do.sleep) {
            az = (atan(data[, 3] / (sqrt(data[, 1]^2 + data[, 2]^2)))) / (pi/180)
            az = slide(x = az, width = 5*sf, FUN = mean)
            anglez = c(anglez, az)
          }
          # Classifier
          act = classify(data = data, sf = sf,
                         classifier = classifier, infoClassifier = infoClassifier,
                         ID = ID, starttime = starttime)
          activity = c(activity, act)
        }
        iteration = iteration + 1
      }
      # Timestamp and ID
      if (!is.null(activity)) {
        timestamp = deriveTimestamps(from = recording_starttime,
                                     length = length(activity), epoch = epoch)
        timestamp = as.data.frame(timestamp)
        subject = rep(ID, length(activity))
        ts = as.data.frame(cbind(subject, timestamp, tilt, activity, nonwear))
        if (length(enmo) == nrow(ts)) ts = as.data.frame(cbind(ts, enmo))
        if (!is.null(agcounts)) {
          if (nrow(agcounts) == nrow(ts)) ts = as.data.frame(cbind(ts, agcounts))
        }
        if (!is.null(LFEcounts)) {
          if (nrow(LFEcounts) == nrow(ts)) ts = as.data.frame(cbind(ts, LFEcounts))
        }
        numeric_columns = sapply(ts, mode) == 'numeric'
        ts[numeric_columns] =  round(ts[numeric_columns], 3)
        ts  = do.call(data.frame,lapply(ts, function(x) replace(x, is.infinite(x), NA)))
        ts[is.na(ts)] = 0
        # classify sleep and nonwear and add them to ts$activity
        if (do.sleep == TRUE | do.nonwear == TRUE) {
          activity = classifySleep(anglez = anglez, starttime = recording_starttime,
                                   classifier = classifier, infoClassifier = infoClassifier,
                                   ts = ts, do.sleep = do.sleep, do.nonwear = do.nonwear)
          ts$activity = activity # overwrite with nighttime, sleep and nonwear information
        }
        # MILESTONE: save features data in features folder
        original_classifier = classifier
        save(ts, original_classifier, file = fn2save)
        # blank line before next file
        if (verbose) cat("\n\n----\n")
      }
    }
  }
  # 6 - aggregate per date (and visualize)
  if (verbose) {
    cat("\n\n")
    cat("Calculating and exporting the summary statistics per day and per recording...\n")
  }
  tsDir = dirname(fn2save)
  daysummary = aggregate_per_date(tsDir = tsDir, epoch = epoch, classifier = classifier,
                                  classes = classes, boutdur = boutdur, boutcriter = boutcriter,
                                  boutmaxgap = boutmaxgap,
                                  visualreport = visualreport)
  fn2save = file.path(output_directory, "results", "daylevel_report.csv")
  data.table::fwrite(daysummary, file = fn2save, na = "", row.names = FALSE)
  # 7 - aggregate per person
  personsummary = aggregate_per_person(daysummary = daysummary,
                                       n_valid_hours = n_valid_hours,
                                       n_valid_hours_awake = n_valid_hours_awake,
                                       n_valid_hours_nighttime = n_valid_hours_nighttime)
  fn2save = file.path(output_directory, "results", "personlevel_report.csv")
  if (!is.null(personsummary)) {
    data.table::fwrite(personsummary, file = fn2save, na = "", row.names = FALSE)
  } else {
    warning("No valid days in recording, no person-level output generated")
  }
  if (verbose) {
    cat("\n\n")
    cat("Done!")
  }
}

