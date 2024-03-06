#' Calculates Features Proposed By Trost 2017
#'
#' @param data Matrix with 3 columns containing the raw acceleration for X, Y,
#' and Z axes in G units.
#' @param epoch Numeric with the epoch length in seconds.
#' @param sf Numeric with the sampling frequency in Hz.
#'
#' @return Matrix with features per epoch as proposed in Trost 2018.
#' @export
#' @references Pavey et al., Journal of Science and Medicine in Sport 2017;20(1): 75-80
featuresTrost2017 = function(data, epoch, sf) {
  window = epoch * sf
  l = floor(nrow(data)/window)
  k = matrix(0, nrow = l, ncol = 16*3)
  column = 1
  for (ai in 1:ncol(data)) {
    Axis = data[, ai]
    from = 1
    to = window
    if (ai > 1) column = column + 16
    for (i in 1:l) {
      A = Axis[from:to]
      N = length(A)
      I = abs(fft(A - mean(A))/sqrt(N))^2
      P = (4/N)*I
      f = (0:(N/2))/N
      c = cbind(f,P[1:(N/2 + 1)])
      c2 = c[1:(window/10),]
      c3 = c[(window/3 + 1):(window/2 + 1),]
      k[i, column] = max(c2[,2])
      k[i, column + 1] = max(c3[,2])
      k[i, column + 2] = sf*(domfreq <- as.numeric(c2[which.max(c2[,2]),1]))
      k[i, column + 3] = sf*(domfreq <- as.numeric(c3[which.max(c3[,2]),1]))
      k[i, column + 4] = mean(A, trim = 0.05, na.rm = TRUE)
      k[i, column + 5] = sd(A, na.rm = TRUE)
      k[i, column + 6] = sum((A*A), na.rm = TRUE)
      k[i, column + 7] = cor(Axis[from:(to - 1)], Axis[(from + 1):to], use = "pairwise.complete.obs")
      k[i, column + 8] = median(A, na.rm = TRUE)
      k[i, column + 9] = mad(abs(A), na.rm = TRUE)
      k[i, column + 10] = as.vector(quantile(A, c(.10), na.rm = TRUE))
      k[i, column + 11] = as.vector(quantile(A, c(.25), na.rm = TRUE))
      k[i, column + 12] = as.vector(quantile(A, c(.75), na.rm = TRUE))
      k[i, column + 13] = as.vector(quantile(A, c(.90), na.rm = TRUE))
      k[i, column + 14] = shannon.entropy(P)[1]
      k[i, column + 15] = sum(c[(window/3 + 1):(window/2 + 1), 2])
      from = from + window
      to = to + window
    }
  }
  # assign column names
  varnames = c("Power253", "Power1315", "DomF253", "DomF1315",
               "Mean", "SD", "SigPow", "LAG1", "Median", "MAD",
               "P10", "P25", "P75", "P90", "Shannon", "Sum1315")
  colnames(k) = c(paste0(varnames, ".X"), paste0(varnames, ".Y"), paste0(varnames, ".Z"))
  return(k)
}

