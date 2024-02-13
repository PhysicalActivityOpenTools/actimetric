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
#' Adult women Hip RF Ellis,
#' Thigh Decision Tree)
#'
#' @return List containing epoch, rfmodel, and hmmmodel
#' @export
#'
#' @examples
#' # example code
#' \dontRun{
#' GetInfoClassifier(classifier = "Preschool Wrist Random Forest Free Living")
#' }
GetInfoClassifier = function(classifier) {
  # initiate objects to return
  epoch = rfmodel = hmmmodel = classes = NULL

  # lowercase classifier to facilitate matching
  classifier = tolower(classifier)

  # define information for classifiers (epoch and models)
  classifierAvailable = TRUE
  if (is.null(classifier)) {
    classifierAvailable = FALSE
  } else {
    # 1 - Preschool and school-age models
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
          rfmodel = preschool_hipfl_15s
        } else if (classifier == "preschool hip random forest free living lag-lead") {
          rfmodel = PS.RF.FL.Hip15.LagLead
        } else {
          classifierAvailable = FALSE
        }
      } else if (grepl("preschool wrist", classifier)) {
        if (classifier == "preschool wrist random forest free living") {
          rfmodel = preschooltest
        } else if (classifier == "preschool wrist random forest free living lag-lead") {
          rfmodel = PS.RF.FL.Wrist15.LagLead
        } else {
          classifierAvailable = FALSE
        }
      } else if (classifier == "school age wrist random forest") {
        rfmodel = LVAY.RF.Wrist5.1
      } else if (classifier == "school age hip random forest") {
        rfmodel = LVAY.RF.Hip10
      } else {
        classifierAvailable = FALSE
      }
      # 2 - Adult Trost (Wrist)
    } else if (classifier == "adult wrist rf trost") {
      classes = c("sedentary", "stationary", "walk", "run")
      epoch = 10
      rfmodel = trostRF_7112014
      # 3 - Adult women Ellis
    } else if (grepl(pattern = "ellis", x = classifier)) {
      classes = c("biking", "sedentary",
                  "standMoving", "standStill", "walk")
      epoch = 60
      if (classifier == "adult women wrist rf ellis") {
        rfmodel = Ellis.Wrist.RF.HMM$rf
        hmmmodel = Ellis.Wrist.RF.HMM$hmm
      } else if (classifier == "adult women hip rf ellis") {
        rfmodel = Ellis.Hip.RF.HMM$rf
        hmmmodel = Ellis.Hip.RF.HMM$hmm
      } else {
        classifierAvailable = FALSE
      }
      # 4 - Thigh
    } else if (classifier == "thigh decision tree") {
      classes = c("sitting", "laying", "standMoving",
                  "walk", "run", "climbStairs", "biking")
      epoch = 10
    } else {
      classifierAvailable = FALSE
    }
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
  return(list(epoch = epoch, rfmodel = rfmodel, hmmmodel = hmmmodel, classes = classes))
}
