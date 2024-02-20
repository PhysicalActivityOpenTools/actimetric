#' Title
#'
#' @param w
#' @param Fs
#'
#' @return
#' @export
#'
#' @importFrom signal specgram
#' @import stats
featuresEllis2016 = function (w, Fs)
{
  g = matrix(0, nrow(w), 3)
  x = 0.9
  g[1, ] = (1 - x) * w[1, ]
  for (n in 2:nrow(w)) {
    g[n, ] = x * g[n - 1] + (1 - x) * w[n, ]
  }
  g = g[Fs:nrow(g), ]
  gg = colMeans(g)
  w = w - gg
  v = sqrt(rowSums(w^2))
  fMean = mean(v)
  fStd = sd(v)
  if (fMean > 0) {
    fCoefVariation = fStd/fMean
  }
  else {
    fCoefVariation = 0
  }
  fMedian = median(v)
  fMin = min(v)
  fMax = max(v)
  f25thP = quantile(v, 0.25)[[1]]
  f75thP = quantile(v, 0.75)[[1]]
  a = acf(v, plot = FALSE)
  fAutocorr = which.max(abs(a$acf[2:length(a$acf)]))/(nrow(w)/Fs)
  if ((sd(w[, 3]) > 0) & (sd(w[, 2]) > 0)) {
    fCorrxy = cor(w[, 3], w[, 2])
  }
  else {
    fCorrxy = 0
  }
  if ((sd(w[, 3]) > 0) & (sd(w[, 1]) > 0)) {
    fCorrxz = cor(w[, 3], w[, 1])
  }
  else {
    fCorrxz = 0
  }
  if ((sd(w[, 2]) > 0) & (sd(w[, 1]) > 0)) {
    fCorryz = cor(w[, 2], w[, 1])
  }
  else {
    fCorryz = 0
  }
  if (is.na(fCorrxy))
    fCorrxy = 0
  if (is.na(fCorrxz))
    fCorrxz = 0
  if (is.na(fCorryz))
    fCorryz = 0
  fAvgRoll = mean(atan2(w[, 2], w[, 1]))
  fAvgPitch = mean(atan2(w[, 1], w[, 3]))
  fAvgYaw = mean(atan2(w[, 2], w[, 3]))
  fSdRoll = sd(atan2(w[, 2], w[, 1]))
  fSdPitch = sd(atan2(w[, 1], w[, 3]))
  fSdYaw = sd(atan2(w[, 2], w[, 3]))
  fRollG = atan2(gg[2], gg[1])
  fPitchG = atan2(gg[1], gg[3])
  fYawG = atan2(gg[2], gg[3])
  # frequency domain
  s = signal::specgram(v, n = length(v), Fs = Fs)
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
  s = signal::specgram(v, n = Fs, Fs = Fs)
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
  return(c(fMean, fStd, fCoefVariation, fMedian, fMin, fMax,
           f25thP, f75thP, fAutocorr, fCorrxy, fCorrxz, fCorryz,
           fAvgRoll, fAvgPitch, fAvgYaw, fSdRoll, fSdPitch, fSdYaw,
           fRollG, fPitchG, fYawG, fFmax, fPmax, fFmaxBand, fPmaxBand,
           fEntropy, FFT0, FFT1, FFT2, FFT3, FFT4, FFT5, FFT6,
           FFT7, FFT8, FFT9, FFT10, FFT11, FFT12, FFT13, FFT14))
}

