#' Classifies Accelerometer Data into Physical Activity Types
#'
#' @description
#' This is the central function of the actimetric package.
#'
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
#' @param sf Number with the sampling frequency in the recording.
#' @param infoClassifier Information relative to the classifier as extracted from \link{GetInfoClassifier}
#' @param ID ID for this recording
#' @param starttime Start time for the recording as extracted from \link{ReadAndCalibrate}
#' @param data Raw data as read by \link{ReadAndCalibrate}
#' @param parameters List with the definition of the parameters of the function.
#' @param remaining_epochs Vector of lenght nrow(data) with information about the epochs that are to be imputed at epoch level.
#' @param tz A character string specifying the time zone to be used for the conversion.
#'   Examples include `"UTC"`, `"America/New_York"`, or `"Europe/Berlin"`.
#'   If not specified, the system's default time zone is used. Time zone handling affects
#'   how character or numeric inputs are interpreted and displayed.
#'   A full list of time zone identifiers can be found on
#'   [Wikipedia](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones).
#'
#' @return Function does not return anything, it only generates the reports and
#' visualizations in the \code{output_directory}.
#' @export
#' @importFrom caret predict.train
#' @importFrom stats predict
#' @importFrom HMM viterbi
#' @import randomForest
#'
#' @author Matthew N. Ahmadi <matthew.ahmadi@sydney.edu.au>
#' @author Jairo H. Migueles <jairo@jhmigueles.com>
classify = function(data = NULL, parameters = NULL, sf = NULL,
                    classifier = NULL, infoClassifier = NULL,
                    ID = NULL, starttime = NULL,
                    remaining_epochs = NULL, tz = "") {
  # -------------------------------------------------------------------------
  # Original code provided by Matthew N. Ahmadi
  # Jairo H. Migueles cleaned the code and isolated the classify function here
  # -------------------------------------------------------------------------
  # get parameters
  if (!is.null(parameters)) {
    if (!"infoClassifier" %in% names(parameters)) {
      parameters$infoClassifier = GetInfoClassifier(parameters$classifier)
    }
    sf = parameters$sf
    classifier = parameters$classifier
    infoClassifier = parameters$infoClassifier
    epoch = parameters$infoClassifier
  }
  epoch = infoClassifier$epoch
  # -------------------------------------------------------------------------
  # 1 - EXTRACT FEATURES
  # Features for classifier
  ts = ExtractFeatures(data, classifier = classifier,
                       epoch = epoch, sf = sf,
                       ID = ID)
  rm(data); gc()
  # impute long gaps, if any
  longgaps2fill = which(remaining_epochs > 1)
  if (length(longgaps2fill) > 0) { # there are periods of the signal to impute
    # last observation carried forward applied by default
    ts = impute_gaps_epoch_level(ts, remaining_epochs = remaining_epochs)
  }
  # Timestamp and ID
  if (!is.null(starttime)) {
    timestamp = deriveTimestamps(from = starttime, length = nrow(ts), epoch = epoch, tz = tz)
    if (!is.null(ID)) subject = rep(ID, nrow(ts)) else subject = NA
    ts = as.data.frame(cbind(subject, timestamp, ts))
  }
  # round numeric columns
  ts  = do.call(data.frame,lapply(ts, function(x) replace(x, is.infinite(x), NA)))
  ts[is.na(ts)] = 0
  # -------------------------------------------------------------------------
  # 3 - APPLY CLASSIFIER
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
    activity = ts$activity
  } else if (is.null(infoClassifier$hmmmodel)) {
    activity = tryCatch(stats::predict(infoClassifier$rfmodel, ts),
                        error = function(e) caret::predict.train(infoClassifier$rfmodel, ts))
  } else {
    # For now, only Ellis classifiers
    testDat = ts[, which(colnames(ts) == "mean"):which(colnames(ts) == "fft14")]
    testDat = predict(infoClassifier$rfmodel$preProcValues, testDat)
    activity = stats::predict(infoClassifier$rfmodel,testDat)
    ts[, 4:44] = testDat
    filtered = HMM::viterbi(infoClassifier$hmmmodel, activity)
    ts = cbind(ts, activity, filtered)
    ts$activity_orig = ts$activity
    ts$hmm_orig = ts$filtered
  }
  # factorize activity
  activity = as.numeric(activity)
  activity = factor(activity, levels = 1:length(infoClassifier$classes),
                    labels = infoClassifier$classes)
  # return
  return(activity)
}

