#' Extracts Features from raw data
#'
#' @param sf Numeric with the sampling frequency in Hz.
#' @param classifier Character specifying the classifier to be used
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
#' @param data Matrix with 3 columns containing the raw acceleration for X, Y,
#' and Z axes in G units.
#' @param epoch Numeric with the epoch length in seconds.
#' @param do.enmo Logical (default = TRUE) indicating whether ENMO should be
#' calculated.
#' @param do.actilifecounts Logical (default = FALSE) indicating whether activity
#' counts should be calculated.
#' @param do.actilifecountsLFE Logical (default = FALSE) indicating whether activity
#' counts using the low-frequency extension filter should be calculated.
#' @param ID Character with ID corresponding to subject
#' @return
#' @export
#' @details
#' This is a central function to calculate features from the acceleration signal.
#' If any of the implemented classifiers is to be used, then the function
#' automatically calculates the features needed for such classifier.
#'
#' @importFrom signal specgram
#' @import stats
ExtractFeatures = function(data, classifier = NULL, sf = NULL, epoch = NULL,
                           do.enmo = TRUE,
                           do.actilifecounts = FALSE, do.actilifecountsLFE = FALSE,
                           ID = NA) {
  classifier = tolower(classifier)
  # initialize variables
  fnames = c()
  enmo = agcounts = LFEcounts = tilt = NULL
  # Basic features ----------------------------------------------------------
  # enmo per epoch
  vm = sqrt(rowSums(data[, 1:3]^2))
  if (do.enmo) {
    enmo = vm - 1
    enmo[which(enmo < 0)] = 0
    enmo = slide(enmo, width = epoch*sf, FUN = mean)
    fnames = c(fnames, "enmo")
  }
  # activity counts per epoch with default filter
  if (do.actilifecounts) {
    agcounts = actilifecounts::get_counts(raw = data[, 1:3], sf = sf, epoch = epoch,
                                          lfe_select = FALSE, verbose = FALSE)
    fnames = c(fnames, "agcounts_x", "agcounts_y", "agcounts_z", "agcounts_vm")
  }
  # activity counts per epoch with LFE
  if (do.actilifecountsLFE) {
    LFEcounts = actilifecounts::get_counts(raw = data[, 1:3], sf = sf, epoch = epoch,
                                           lfe_select = TRUE, verbose = FALSE)
    fnames = c(fnames, "LFEcounts_x", "LFEcounts_y", "LFEcounts_z", "LFEcounts_vm")
  }
  # tilt
  tilt = acos(data[,2]/vm)*(180/pi)
  tilt = slide(tilt, width = epoch*sf, FUN = mean)
  fnames = c(fnames, "tilt")
  # -------------------------------------------------------------------------
  # Extract features needed for each classifier
  classifierAvailable = TRUE
  if (is.null(classifier)) {
    classifierAvailable = FALSE
  } else {
    if (grepl(pattern = "preschool|school age", x = classifier)) {
      features = featuresTrost2018(data = data[, 1:3], vm = vm,
                                   epoch = epoch, sf = sf,
                                   overlap = 0, lowerBound = 0.25, upperBound = 5)
    } else if (classifier == "adult wrist rf trost") {
      features = featuresTrost2017(data = data[, 1:3], epoch = epoch, sf = sf)
    } else if (grepl(pattern = "ellis", x = classifier)) {
      features = featuresEllis()
    } else if (classifier == "thigh decision tree") {
      features = featuresThigh()
    } else {
      classifierAvailable = FALSE
    }
    # lag-lead features
    # if (grepl(pattern = "lag-lead", x = classifier)) {
    #   lagsd1 = dplyr::lag(hold[,2],default = 0)
    #   lagsd2 = dplyr::lag(hold[,2],default = 0,n=2)
    #   leadsd1 = dplyr::lead(hold[,2],default = 0)
    #   leadsd2 = dplyr::lead(hold[,2],default = 0,n=2)
    #   laglead = cbind(lagsd1,lagsd2,leadsd1,leadsd2)
    #   combsd = apply(cbind(laglead,hold[,2]),1,sd)
    #   hold = cbind(hold[,1:26],laglead,combsd,hold[,27:29])
    # }
  }
  # run error if classifier was not found
  if (classifierAvailable == FALSE) {
    stop(paste0("The defined classifier is not within our current alternatives.\n\n",
                " Please copy/paste one of the following clasifiers:\n",
                "    - Preschool Wrist Random Forest Free Living\n",
                "    - Preschool Hip Random Forest Free Living\n",
                "    - Preschool Hip Random Forest Free Living Lag-Lead\n",
                "    - Preschool Wrist Random Forest Free Living Lag-Lead\n",
                "    - School age Wrist Random Forest\n",
                "    - School age Hip Random Forest\n",
                "    - Adult Wrist RF Trost\n",
                "    - Adult women Wrist RF Ellis\n",
                "    - Adult women Hip RF Ellis\n",
                "    - Thigh Decision Tree\n"))
  }
  # merge basic features with features
  fnames = c(colnames(features), fnames)
  features = as.data.frame(cbind(features, enmo, agcounts, LFEcounts, tilt))
  colnames(features) = fnames
  rownames(features) = 1:nrow(features)
  return(features)
}
