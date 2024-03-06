#' Calculates Features Proposed By Ellis 2016
#'
#' @param epoch Numeric with the epoch length in seconds.
#' @param sf Numeric with the sampling frequency in Hz.
#' @param x Numeric vector with the raw acceleration for X axis in G units.
#' @param y Numeric vector with the raw acceleration for Y axis in G units.
#' @param z Numeric vector with the raw acceleration for Z axis in G units.
#'
#' @return Matrix with features per epoch as proposed in Ellis 2016.
#' @export
#' @references Ellis et al., Medicine and Science in Sport and Exercise 2016;48(5): 933-940
#'
#' @importFrom signal specgram
#' @import stats
featuresEllis2016 = function(x, y, z, epoch, sf) {
  data = cbind(x, y, z)
  g = matrix(0, nrow(data), 3)
  x = 0.9
  g[1, ] = (1 - x) * data[1, ]
  for (n in 2:nrow(data)) g[n, ] = x * g[n - 1] + (1 - x) * data[n, ]
  g = g[sf:nrow(g), ]
  gg = colMeans(g)
  data = data - gg
  v = sqrt(rowSums(data^2))
  fMean = mean(v)
  fStd = sd(v)
  fCoefVariation = ifelse(test = fMean > 0, yes = fStd/fMean, no = 0)
  fMedian = median(v)
  fMin = min(v)
  fMax = max(v)
  f25thP = quantile(v, 0.25)[[1]]
  f75thP = quantile(v, 0.75)[[1]]
  a = acf(v, plot = FALSE)
  fAutocorr = which.max(abs(a$acf[2:length(a$acf)]))/(nrow(data)/sf)
  fCorrxy = ifelse(test = (sd(data[, 3]) > 0) & (sd(data[, 2]) > 0),
                   yes = cor(data[, 3], data[, 2]), no = 0)
  fCorrxz = ifelse(test = (sd(data[, 3]) > 0) & (sd(data[, 1]) > 0),
                   yes = cor(data[, 3], data[, 1]), no = 0)
  fCorryz = ifelse(test = (sd(data[, 2]) > 0) & (sd(data[, 1]) > 0),
                   yes = cor(data[, 2], data[, 1]), no = 0)
  if (is.na(fCorrxy)) fCorrxy = 0
  if (is.na(fCorrxz)) fCorrxz = 0
  if (is.na(fCorryz)) fCorryz = 0
  fAvgRoll = mean(atan2(data[, 2], data[, 1]))
  fAvgPitch = mean(atan2(data[, 1], data[, 3]))
  fAvgYaw = mean(atan2(data[, 2], data[, 3]))
  fSdRoll = sd(atan2(data[, 2], data[, 1]))
  fSdPitch = sd(atan2(data[, 1], data[, 3]))
  fSdYaw = sd(atan2(data[, 2], data[, 3]))
  fRollG = atan2(gg[2], gg[1])
  fPitchG = atan2(gg[1], gg[3])
  fYawG = atan2(gg[2], gg[3])
  # frequency domain
  s = signal::specgram(v, n = length(v), Fs = sf)
  S = abs(s$S)
  f = S/max(S)
  freq = s$f
  f1 = f[freq >= 0.1]
  freq1 = freq[freq >= 0.1]
  fFmax = freq1[which.max(f1)]
  fPmax = max(f1)
  band = f[freq > 0.3 & freq < 3]
  fPmaxBand = max(band)
  freqband = freq[freq > 0.3 & freq < 3]
  fFmaxBand = freqband[which.max(band)]
  fEntropy = -sum(f * log(f))
  s = signal::specgram(v, n = sf, Fs = sf)
  S = abs(s$S)
  f = S/max(S)
  freq = s$f
  f = rowSums(f)/ncol(f)
  FFT0 = f[1]
  FFT1 = f[2]
  FFT2 = f[3]
  FFT3 = f[4]
  FFT4 = f[5]
  FFT5 = f[6]
  FFT6 = f[7]
  FFT7 = f[8]
  FFT8 = f[9]
  FFT9 = f[10]
  FFT10 = f[11]
  FFT11 = f[12]
  FFT12 = f[13]
  FFT13 = f[14]
  FFT14 = f[15]
  out = c(fMean, fStd, fCoefVariation, fMedian, fMin, fMax,
          f25thP, f75thP, fAutocorr, fCorrxy, fCorrxz, fCorryz,
          fAvgRoll, fAvgPitch, fAvgYaw, fSdRoll, fSdPitch, fSdYaw,
          fRollG, fPitchG, fYawG, fFmax, fPmax, fFmaxBand, fPmaxBand,
          fEntropy, FFT0, FFT1, FFT2, FFT3, FFT4, FFT5, FFT6,
          FFT7, FFT8, FFT9, FFT10, FFT11, FFT12, FFT13, FFT14)
  names(out) = c("mean", "sd", "coefvariation", "median", "min", "max",
                 "X25thp", "X75thp", "autocorr", "corrxy", "corrxz", "corryz",
                 "avgroll", "avgpitch", "avgyaw", "sdroll", "sdpitch", "sdyaw",
                 "rollg", "pitchg", "yawg", "fmax", "pmax", "fmaxband", "pmaxband",
                 "entropy", "fft0", "fft1", "fft2", "fft3", "fft4", "fft5", "fft6",
                 "fft7", "fft8", "fft9", "fft10", "fft11", "fft12", "fft13", "fft14")
  return(out)
}

