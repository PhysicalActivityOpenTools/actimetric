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
classify = function(data = NULL, sf = NULL,
                    classifier = NULL, infoClassifier = NULL,
                    ID = NULL, starttime = NULL) {
  # Original code provided by Matthew N. Ahmadi
  # Jairo H. Migueles cleaned the code and isolated the classify function here
  epoch = infoClassifier$epoch
  # -------------------------------------------------------------------------
  # 1 - EXTRACT FEATURES
  # Features for classifier
  ts = ExtractFeatures(data, classifier = classifier,
                       epoch = epoch, sf = sf,
                       ID = ID)
  ts = as.data.frame(ts)
  rm(data); gc()
  # Lag-lead features if needed
  if (grepl("lag-lead", classifier, ignore.case = TRUE)) {
    lagsd1 = c(0, ts$vm.sd[1:c(nrow(ts) - 1)])
    lagsd2 = c(0, 0, ts$vm.sd[1:c(nrow(ts) - 2)])
    leadsd1 = c(ts$vm.sd[2:nrow(ts)], 0)
    leadsd2 = c(ts$vm.sd[3:nrow(ts)], 0, 0)
    combsd = apply(cbind(lagsd1, lagsd2, leadsd1, leadsd2), 1, sd)
    laglead = cbind(lagsd1, lagsd2, leadsd1, leadsd2, combsd)
    ts = as.data.frame(cbind(ts, laglead))
  }
  # Timestamp and ID
  timestamp = deriveTimestamps(from = starttime, length = nrow(ts), epoch = epoch)
  subject = rep(ID, nrow(ts))
  ts = as.data.frame(cbind(subject, timestamp, ts))
  numeric_columns = sapply(ts, mode) == 'numeric'
  ts[numeric_columns] =  round(ts[numeric_columns], 3)
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

