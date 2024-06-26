% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classify.R
\name{classify}
\alias{classify}
\title{Classifies Accelerometer Data into Physical Activity Types}
\usage{
classify(
  data = NULL,
  parameters = NULL,
  sf = NULL,
  classifier = NULL,
  infoClassifier = NULL,
  ID = NULL,
  starttime = NULL
)
}
\arguments{
\item{data}{Raw data as read by \link{ReadAndCalibrate}}

\item{parameters}{List with the definition of the parameters of the function.}

\item{sf}{Number with the sampling frequency in the recording.}

\item{classifier}{Character (default = NULL) indicating the classifier to be used
(available options are:
Preschool Wrist Random Forest Free Living,
Preschool Hip Random Forest Free Living,
Preschool Hip Random Forest Free Living Lag-Lead,
Preschool Wrist Random Forest Free Living Lag-Lead,
School age Wrist Random Forest, School age Hip Random Forest,
Adult Wrist RF Trost,
Adult women Wrist RF Ellis,
Adult women Hip RF Ellis)}

\item{infoClassifier}{Information relative to the classifier as extracted from \link{GetInfoClassifier}}

\item{ID}{ID for this recording}

\item{starttime}{Start time for the recording as extracted from \link{ReadAndCalibrate}}
}
\value{
Function does not return anything, it only generates the reports and
visualizations in the \code{output_directory}.
}
\description{
This is the central function of the actimetric package.
}
\author{
Matthew N. Ahmadi <matthew.ahmadi@sydney.edu.au>

Jairo H. Migueles <jairo@jhmigueles.com>
}
