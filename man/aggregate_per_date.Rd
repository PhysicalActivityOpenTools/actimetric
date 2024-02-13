% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_per_date.R
\name{aggregate_per_date}
\alias{aggregate_per_date}
\title{Aggregate Per Date}
\usage{
aggregate_per_date(ts, epoch, classifier, classes, boutdur, boutcriter)
}
\arguments{
\item{ts}{Data frame with ts object from \link{extractFeatures}}

\item{epoch}{Numeric with the epoch length in seconds.}

\item{classifier}{Character (default = NULL) indicating the classifier to be used
(available options are:
Preschool Wrist Random Forest Free Living,
Preschool Hip Random Forest Free Living,
Preschool Hip Random Forest Free Living Lag-Lead,
Preschool Wrist Random Forest Free Living Lag-Lead,
School age Wrist Random Forest, School age Hip Random Forest,
Adult Wrist RF Trost,
Adult women Wrist RF Ellis,
Adult women Hip RF Ellis,
Thigh Decision Tree)}

\item{classes}{Character (default = NULL) indicating the behavioural classes estimated by the classifier}

\item{boutdur}{Numeric vector (default = c(1, 10, 30)) indicating the bout durations over which calculate bouts of behaviors}

\item{boutcriter}{Numeric (default = 0.8) indicating the proportion of the bout duration that should be classified in a given behavior to consider a bout}
}
\description{
Aggregate Per Date
}