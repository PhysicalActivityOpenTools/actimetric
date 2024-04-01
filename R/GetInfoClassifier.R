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
        rfmodel = actimetricModels::preschool_hipfl_15s
      } else if (classifier == "preschool hip random forest free living lag-lead") {
        rfmodel = actimetricModels::PS.RF.FL.Hip15.LagLead
      }
    } else if (grepl("preschool wrist", classifier)) {
      if (classifier == "preschool wrist random forest free living") {
        rfmodel = actimetricModels::preschooltest
      } else if (classifier == "preschool wrist random forest free living lag-lead") {
        rfmodel = actimetricModels::PS.RF.FL.Wrist15.LagLead
      }
    } else if (classifier == "school age wrist random forest") {
      rfmodel = actimetricModels::LVAY.RF.Wrist5.1
    } else if (classifier == "school age hip random forest") {
     rfmodel = actimetricModels::LVAY.RF.Hip10
    }
    # 2 - Adult Trost (Wrist)
  } else if (classifier == "adult wrist rf trost") {
    classes = c("sedentary", "stationary", "walk", "run")
    epoch = 10
    rfmodel = actimetricModels::trostRF_7112014
    # 3 - Adult women Ellis
  } else if (grepl(pattern = "ellis", x = classifier)) {
    classes = c("biking", "sedentary", "standMoving", "standStill", "vehicle","walk")
    epoch = 60
    if (classifier == "adult women wrist rf ellis") {
      rfmodel = actimetricModels::Ellis.Wrist.RF.HMM$rf
      hmmmodel = actimetricModels::Ellis.Wrist.RF.HMM$hmm
    } else if (classifier == "adult women hip rf ellis") {
     rfmodel = actimetricModels::Ellis.Hip.RF.HMM$rf
      hmmmodel = actimetricModels::Ellis.Hip.RF.HMM$hmm
    }
    # 4 - Thigh
  } else if (classifier == "thigh decision tree") {
    classes = c("sit", "stand", "standMove",
                "walk", "run", "climbStairs", "bike")
    epoch = 10
  }
  return(list(epoch = epoch, rfmodel = rfmodel, hmmmodel = hmmmodel, classes = classes))
}
