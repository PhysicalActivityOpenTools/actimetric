#' Checks the Spelling and Selects the Correct Classifier
#'
#' @description
#' Tries to match the classifier specified by the user with one of the
#' supported options in the package. If not successful returns NULL.
#'
#'
#' @param classifier Character with user-specified classifier.
#'
#' @return Character with the definitive classifier
#' @export
#'
#' @examples
#' @author Jairo H. Migueles <jairo@jhmigueles.com>
#' check_classifier("Adult wrist Trost")
check_classifier = function(classifier) {
  # lowercase classifier to facilitate matching
  user_classifier = tolower(classifier)
  classifier = NULL
  # 1 - Preschool and school-age models
  preschool_pattern = c("preschool|pre-school|pre school")
  child_pattern = c("school age|child|children|boy|girl|adolescent|teenage")
  adult_pattern = c("adult|men|women")
  wrist_pattern = c("wrist|hand|arm")
  hip_pattern = c("hip|waist")
  thigh_pattern = c("thigh|leg")
  # preschool classifiers
  if (grepl(pattern = preschool_pattern, x = user_classifier)) {
    if (grepl(pattern = wrist_pattern, x = user_classifier)) {
      if (grepl(pattern = "lag-lead|laglead|lag lead", x = user_classifier)) {
        classifier = "Preschool wrist random forest free living lag-lead"
      } else {
        classifier = "Preschool wrist random forest free living"
      }
    } else if (grepl(pattern = hip_pattern, x = user_classifier)) {
      if (grepl(pattern = "lag-lead|laglead|lag lead", x = user_classifier)) {
        classifier = "Preschool hip random forest free living lag-lead"
      } else {
        classifier = "Preschool hip random forest free living"
      }
    }
    # 2 - school age
  } else if (grepl(pattern = child_pattern, x = user_classifier)) {
    if (grepl(pattern = wrist_pattern, x = user_classifier)) {
      classifier = "School age wrist random forest"
    } else if (grepl(pattern = hip_pattern, x = user_classifier)) {
      classifier = "School age hip random forest"
    }
    # 3 - adults
  } else if (grepl(pattern = adult_pattern, x = user_classifier)) {
    if (grepl(pattern = "trost", x = user_classifier)) {
      classifier = "Adult wrist RF Trost"
    } else if (grepl(pattern = "ellis", x = user_classifier)) {
      if (grepl(pattern = wrist_pattern, x = user_classifier)) {
        classifier = "Adult women wrist RF Ellis"
      } else if (grepl(pattern = hip_pattern, x = user_classifier)) {
        classifier = "Adult women hip RF Ellis"
      }
    }
  }
  return(classifier)
}
