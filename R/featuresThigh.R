#' Calculates Features for Thigh classifier
#'
#' @param epoch Numeric with the epoch length in seconds.
#' @param sf Numeric with the sampling frequency in Hz.
#' @param x Numeric vector with the raw acceleration for X axis in G units.
#' @param y Numeric vector with the raw acceleration for Y axis in G units.
#' @param z Numeric vector with the raw acceleration for Z axis in G units.
#'
#' @return Matrix with features per epoch as proposed in Ellis 2016.
#' @export
#'
#' @import stats
#' @author Matthew N. Ahmadi <matthew.ahmadi@sydney.edu.au>
featuresThigh = function(x, y, z, epoch, sf) {
  # thigh models are only experimental and disabled for now.
  # Internal functions ------------------------------------------------------
  medianCrossing = function(x) {
    q = as.numeric(quantile(x, p = 0.5))
    bb = ifelse(x > q, yes = 1, no = 0)
    d = diff(bb)
    e = which(d > 0)
    e = as.numeric(length(e))
    bb = ifelse(x < q, 1, 0)
    d = diff(bb)
    f = which(d > 0)
    f = as.numeric(length(f))
    return(f + e)
  }

  # main code ---------------------------------------------------------------
  # # matrices of epoch x time
  # ax = matrix(x, nrow = epoch * sf, ncol = ceiling(length(x)/(epoch * sf)), byrow = F)
  # ay = matrix(y, nrow = epoch * sf, ncol = ceiling(length(y)/(epoch * sf)), byrow = F)
  # az = matrix(z, nrow = epoch * sf, ncol = ceiling(length(z)/(epoch * sf)), byrow = F)
  # # calculate features and store in out
  # expectedEpochs = dim(ax)[2]
  ii = 1
  out = rep(0, times = 15)
  # while (ii <= expectedEpochs) {
  ################################################
  # x = signal::filter(bf,ax[,ii] )
  # y = signal::filter(bf, ay[,ii])
  # z = signal::filter(bf, az[,ii])
  ################################################
  # x =  ax[,ii]
  # y =  ay[,ii]
  # z =  az[,ii]
  bfvm = sqrt((x^2) + (y^2) + (z^2))
  # mean and sd ----------------------
  out[1] =  mean(bfvm)
  out[2] = sd(bfvm)
  out[3] = sd(y)
  out[4] = sd(x)
  out[5] = sd(z)

  # angles ------------------------
  out[6] = mean(acos(y/bfvm)*(180/pi))
  out[7] = mean(acos(x/bfvm)*(180/pi))
  out[8] = mean(acos(z/bfvm)*(180/pi))
  out[9] = mean(-asin(y/bfvm)*(180/pi))
  out[10] = mean(-asin(x/bfvm)*(180/pi))
  out[11] = mean(-asin(z/bfvm)*(180/pi))

  # median crossing calculations -----
  out[12] = medianCrossing(x)
  out[13] = medianCrossing(y)
  out[14] = medianCrossing(z)
  out[15] = medianCrossing(bfvm)
  # # next iteration ------
  # ii = ii + 1
  # }
  names(out) = c("bfvm", "bfsd", "bfsd.y", "bfsd.x", "bfsd.z",
                 "inc.y", "inc.x", "inc.z", "fb.y", "fb.x", "fb.z",
                 "median.x", "median.y", "median.z", "median.vm")
  return(out)
}


