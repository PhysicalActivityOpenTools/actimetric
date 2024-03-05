% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/featuresEllis2016.R
\name{featuresEllis2016}
\alias{featuresEllis2016}
\title{Calculates Features Proposed By Ellis 2016}
\usage{
featuresEllis2016(x, y, z, epoch, sf)
}
\arguments{
\item{x}{Numeric vector with the raw acceleration for X axis in G units.}

\item{y}{Numeric vector with the raw acceleration for Y axis in G units.}

\item{z}{Numeric vector with the raw acceleration for Z axis in G units.}

\item{epoch}{Numeric with the epoch length in seconds.}

\item{sf}{Numeric with the sampling frequency in Hz.}
}
\value{
Matrix with features per epoch as proposed in Ellis 2016.
}
\description{
Calculates Features Proposed By Ellis 2016
}
\references{
Ellis et al., Medicine and Science in Sport and Exercise 2016;48(5): 933-940
}