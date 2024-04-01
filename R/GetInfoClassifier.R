#' Get Information To Apply Classifiers
#' @description
#' Gets the required information to run a classifier (i.e., epoch and models)
#'
#' @param classifier Character specifying the classifier to be used
#' (available options are:
#' Preschool Wrist Random Forest Free Living,
#' Preschool Hip Random Forest Free Living,
#' Preschool Hip Random Forest Free Living Lag-Lead,
#' Preschool Wrist Random Forest Free Living Lag-Lead,
#' School age Wrist Random Forest, School age Hip Random Forest,
#' Adult Wrist RF Trost,
#' Adult women Wrist RF Ellis,
#' Adult women Hip RF Ellis)
#'
#' @return List containing epoch, rfmodel, and hmmmodel
#' @import actimetricModels
#' @importFrom utils data
#' @export
GetInfoClassifier = function(classifier) {
  # initiate objects to return
  epoch = rfmodel = hmmmodel = classes = NULL

  # lowercase classifier to facilitate matching
  classifier = tolower(classifier)

  if (grepl(pattern = "preschool|school age", x = classifier)) {
    classes = c("sedentary", "lightGames", "moderatetovigorousGames",
                "walk", "run")
    if (classifier == "school age hip random forest") {
      epoch = 10
    } else {
      epoch = 15
    }
    if (grepl("preschool hip", classifier)) {
      if (classifier == "preschool hip random forest free living") {
        utils::data("preschool_hipfl_15s", package = "actimetricModels")
        rfmodel = force(preschool_hipfl_15s)
        rfmodel = preschool_hipfl_15s
      } else if (classifier == "preschool hip random forest free living lag-lead") {
        utils::data("PS.RF.FL.Hip15.LagLead", package = "actimetricModels")
        rfmodel = force(PS.RF.FL.Hip15.LagLead)
        rfmodel = PS.RF.FL.Hip15.LagLead
      }
    } else if (grepl("preschool wrist", classifier)) {
      if (classifier == "preschool wrist random forest free living") {
        utils::data("preschooltest", package = "actimetricModels")
        rfmodel = force(preschooltest)
        rfmodel = preschooltest
      } else if (classifier == "preschool wrist random forest free living lag-lead") {
        utils::data("PS.RF.FL.Wrist15.LagLead", package = "actimetricModels")
        rfmodel = force(PS.RF.FL.Wrist15.LagLead)
        rfmodel = PS.RF.FL.Wrist15.LagLead
      }
    } else if (classifier == "school age wrist random forest") {
      utils::data("LVAY.RF.Wrist5.1", package = "actimetricModels")
      rfmodel = force(LVAY.RF.Wrist5.1)
      rfmodel = LVAY.RF.Wrist5.1
    } else if (classifier == "school age hip random forest") {
      utils::data("LVAY.RF.Hip10", package = "actimetricModels")
      rfmodel = force(LVAY.RF.Hip10)
      rfmodel = LVAY.RF.Hip10
    }
    # 2 - Adult Trost (Wrist)
  } else if (classifier == "adult wrist rf trost") {
    classes = c("sedentary", "stationary", "walk", "run")
    epoch = 10
    utils::data("trostRF_7112014", package = "actimetricModels")
    rfmodel = force(trostRF_7112014)
    rfmodel = trostRF_7112014
    # 3 - Adult women Ellis
  } else if (grepl(pattern = "ellis", x = classifier)) {
    classes = c("biking", "sedentary", "standMoving", "standStill", "vehicle","walk")
    epoch = 60
    if (classifier == "adult women wrist rf ellis") {
      utils::data("Ellis.Wrist.RF.HMM", package = "actimetricModels")
      Ellis = force(Ellis.Wrist.RF.HMM)
      rfmodel = Ellis$rf
      hmmmodel = Ellis$hmm
    } else if (classifier == "adult women hip rf ellis") {
      utils::data("Ellis.Hip.RF.HMM", package = "actimetricModels")
      Ellis = force(Ellis.Hip.RF.HMM)
      rfmodel = Ellis$rf
      hmmmodel = Ellis$hmm
    }
    # 4 - Thigh
  } else if (classifier == "thigh decision tree") {
    classes = c("sit", "stand", "standMove",
                "walk", "run", "climbStairs", "bike")
    epoch = 10
  }
  return(list(epoch = epoch, rfmodel = rfmodel, hmmmodel = hmmmodel, classes = classes))
}
