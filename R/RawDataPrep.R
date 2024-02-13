#' Title
#'
#' @param classifier
#' @param raw
#'
#' @return
#' @export
#'
#' @examples
RawDataPrep = function(raw, classifier) {
  # lowercase classifier to facilitate matching
  classifier = tolower(classifier)

  # Raw data preparation for classifier
  classifierAvailable = TRUE
  if (is.null(classifier)) {
    classifierAvailable = FALSE
  } else {
    if (grepl(pattern = "preschool|school age", x = classifier)) {

    } else if (classifier == "adult wrist rf trost") {

    } else if (grepl(pattern = "ellis", x = classifier)) {

    } else if (classifier == "thigh decision tree") {

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
  return()
}
