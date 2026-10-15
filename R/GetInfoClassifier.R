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
#' @references Preschool models: Ahmadi MN, Pavey TG, Trost SG. Machine Learning
#' Models for Classifying Physical Activity in Free-Living Preschool Children.
#' Sensors (Basel). 2020 Aug 5;20(16):4364. doi: 10.3390/s20164364
#' @references School age Wrist Random Forest: Chowdhury AK, Tjondronegoro D,
#' Chandran V, Trost SG. Ensemble Methods for Classification of Physical Activities
#' from Wrist Accelerometry. Med Sci Sports Exerc. 2017 Sep;49(9):1965-1973.
#' doi: 10.1249/MSS.0000000000001291
#' @references School age Hip Random Forest: Ahmadi MN, Pfeiffer KA, Trost SG.
#' Physical Activity Classification in Youth Using Raw Accelerometer Data from
#' the Hip. Meas Phys Edu Exerc Sci. 2020; 24(2):129-136. doi: 10.1080/1091367X.2020.1716768
#' @references Adult Wrist RF Trost: Pavey TG, Gilson ND, Gomersall SR, Clark B,
#' Trost SG. Field evaluation of a random forest activity classifier for wrist-worn
#' accelerometer data. J Sci Med Sport. 2017 Jan;20(1):75-80.
#' doi: 10.1016/j.jsams.2016.06.003
#' @references Adult women Wrist and Hip RF Ellis: Ellis K, Kerr J, Godbole S,
#' Staudenmayer J, Lanckriet G. Hip and Wrist Accelerometer Algorithms for
#' Free-Living Behavior Classification. Med Sci Sports Exerc. 2016 May;48(5):933-40.
#' doi: 10.1249/MSS.0000000000000840
#'
#' @author Jairo H. Migueles <jairo@jhmigueles.com>
#' @export
#' @import randomForest
GetInfoClassifier <- function(classifier) {
  # Normalize input
  classifier <- tolower(trimws(classifier))

  # Standard class sets
  classes_youth            <- c("sedentary", "lightGames", "moderatetovigorousGames", "walk", "run")
  classes_school_age_wrist <- c("sedentary", "lightGames", "walk", "run", "moderatetovigorousGames")
  classes_trost            <- c("sedentary", "stationary", "walk", "run")
  classes_ellis            <- c("biking", "sedentary", "standMoving", "standStill", "vehicle", "walk")
  classes_thigh            <- c("sit", "stand", "standMove", "walk", "run", "climbStairs", "bike")

  info <- switch(
    classifier,

    # -------------------------------------------------------------------------
    # Preschool Models
    # -------------------------------------------------------------------------
    "preschool hip random forest free living" = list(
      epoch    = 15,
      rfmodel  = actimetricModels::preschool_hipfl_15s,
      hmmmodel = NULL,
      classes  = classes_youth
    ),

    "preschool hip random forest free living lag-lead" = list(
      epoch    = 15,
      rfmodel  = actimetricModels::PS.RF.FL.Hip15.LagLead,
      hmmmodel = NULL,
      classes  = classes_youth
    ),

    "preschool wrist random forest free living" = list(
      epoch    = 15,
      rfmodel  = actimetricModels::preschooltest,
      hmmmodel = NULL,
      classes  = classes_youth
    ),

    "preschool wrist random forest free living lag-lead" = list(
      epoch    = 15,
      rfmodel  = actimetricModels::PS.RF.FL.Wrist15.LagLead,
      hmmmodel = NULL,
      classes  = classes_youth
    ),

    # -------------------------------------------------------------------------
    # School-Age Models
    # -------------------------------------------------------------------------
    "school age wrist random forest" = list(
      epoch    = 15,
      rfmodel  = actimetricModels::LVAY.RF.Wrist5.1,
      hmmmodel = NULL,
      classes  = classes_school_age_wrist
    ),

    "school age hip random forest" = list(
      epoch    = 10,
      rfmodel  = actimetricModels::LVAY.RF.Hip10,
      hmmmodel = NULL,
      classes  = classes_youth
    ),

    # -------------------------------------------------------------------------
    # Adult Trost Models
    # -------------------------------------------------------------------------
    "adult wrist rf trost" = list(
      epoch    = 10,
      rfmodel  = actimetricModels::trostRF_7112014,
      hmmmodel = NULL,
      classes  = classes_trost
    ),

    # -------------------------------------------------------------------------
    # Adult Ellis Models
    # -------------------------------------------------------------------------
    "adult women wrist rf ellis" = list(
      epoch    = 60,
      rfmodel  = actimetricModels::Ellis.Wrist.RF.HMM$rf,
      hmmmodel = actimetricModels::Ellis.Wrist.RF.HMM$hmm,
      classes  = classes_ellis
    ),

    "adult women hip rf ellis" = list(
      epoch    = 60,
      rfmodel  = actimetricModels::Ellis.Hip.RF.HMM$rf,
      hmmmodel = actimetricModels::Ellis.Hip.RF.HMM$hmm,
      classes  = classes_ellis
    ),

    # -------------------------------------------------------------------------
    # Thigh Models
    # -------------------------------------------------------------------------
    "thigh decision tree" = list(
      epoch    = 10,
      rfmodel  = NULL,
      hmmmodel = NULL,
      classes  = classes_thigh
    ),

    # Default fallback for unrecognized classifiers
    list(
      epoch    = NULL,
      rfmodel  = NULL,
      hmmmodel = NULL,
      classes  = NULL
    )
  )

  return(info)
}
