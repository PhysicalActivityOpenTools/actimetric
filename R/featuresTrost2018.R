#' Calculates Features Proposed By Trost 2018
#'
#' @param data Matrix with 3 columns containing the raw acceleration for X, Y,
#' and Z axes in G units.
#' @param epoch Numeric with the epoch length in seconds.
#' @param sf Numeric with the sampling frequency in Hz.
#' @param lowerBound Lower bound for the frequency filter in Hz.
#' @param upperBound Upper bound for the frequency filter in Hz.
#' @param overlap Whether to overlap windows in the features calculation.
#' @param vm Numeric with vector magnitude of the raw accelerations.
#'
#' @return Matrix with features per epoch as proposed in Trost 2018.
#' @export
#' @references Trost et al., Medicine and Science in Sport and Exercise 2018;50(3): 634-641
#'
#' @import stats
#' @importFrom e1071 skewness kurtosis
featuresTrost2018 = function(data, vm, epoch, overlap, sf,
                             lowerBound = 0.25, upperBound = 5) {
  # INTERNAL FUNCTIONS (only used here) -------------------------------------
  freq.domain = function(data, sf, lowerBound = -Inf, upperBound = Inf) {
    # prepare data
    N = as.numeric(length(data))
    I = abs(stats::fft(data - mean(data))/sqrt(N))^2
    P = ((4/N)*I)^0.5
    f = (0:(N/2))/N
    c = cbind(f*sf,P[1:(N/2 + 1)])
    c = c[c[,1] >= lowerBound & c[,1] <= upperBound,]
    # calculate features
    FMmax = max(c[,2])
    Fmax = (domfreq = as.numeric(c[which.max(c[,2]),1]))
    if (length(Fmax) == 0) Fmax = 0
    entropy = -sum(P * log2(P)) # SHANNON ENTROPY
    freq.feat = cbind(Fmax, FMmax, entropy)
    colnames(freq.feat) = paste0("vm.", colnames(freq.feat), "_",
                                 lowerBound, ".", upperBound, "hz")
    colnames(freq.feat) = gsub("vm.entropy", "Entropy", colnames(freq.feat))
    return(freq.feat)
  }
  time.domain = function(vm) {
    vm.mean = mean(vm); vm.sd = sd(vm); vm.cv = vm.sd/vm.mean
    perc = rbind(quantile(vm, p = c(0.10, 0.25, 0.50, 0.75, 0.90)))
    colnames(perc) = c(paste0("vm.p", gsub("%", "", colnames(perc))))
    vm.skew = e1071::skewness(vm)
    if (is.na(vm.skew)) vm.skew = 0
    vm.kurt = e1071::kurtosis(vm)
    if (is.na(vm.kurt)) vm.kurt = 0
    vm.max = max(vm); vm.min = min(vm); vm.p2p = vm.max - vm.min
    # median crossing -----
    median = as.numeric(perc[,"vm.p50"])
    b = ifelse(vm > median, 1, 0)
    d = diff(b)
    e = which(d > 0)
    e = as.numeric(length(e))
    b = ifelse(vm < median, 1, 0)
    d = diff(b)
    f = which(d > 0)
    f = as.numeric(length(f))
    vm.median = f + e
    # median crossing end -----
    sum = sum(vm); mad = stats::mad(vm); vm.power = sum(vm^2)
    vm.ACF = stats::acf(vm, plot = FALSE, lag.max = 1)$acf[1 + 1]
    if (is.na(vm.ACF)) vm.ACF = 0
    vm.logen = sum(log(vm^2))
    vm.iqr = perc[,"vm.p75"] - perc[,"vm.p25"]
    # return
    features = cbind(vm.mean, vm.sd, vm.cv, perc, vm.skew, vm.kurt,
                     vm.max, vm.min, vm.p2p, vm.median,
                     sum, mad, vm.power, vm.ACF, vm.logen, vm.iqr)
    return(features)
  }
  cross.corr = function(a, b, width, by) {
    # a
    lenX = length(a)
    QUT1 = seq(1, lenX - width + 1, by = by)
    QUT1.1 = seq(width, lenX, by = by)
    a = sapply(1:length(QUT1), function(i) a[QUT1[i]:QUT1.1[i]])
    colnames(a) = c(1:dim(a)[2])
    # b
    lenX = length(b)
    QUT1 = seq(1, lenX - width + 1, by = by)
    QUT1.1 = seq(width, lenX, by = by)
    b = sapply(1:length(QUT1), function(i) b[QUT1[i]:QUT1.1[i]])
    colnames(b) = c(1:dim(b)[2])
    # columns
    cols = intersect(colnames(a), colnames(b))
    # For each column, compute cor
    res = lapply(cols, function(x) cor(a[, x], b[, x]))
    res[is.na(res)] = 0
    return(do.call(rbind, res))
  }
  # MAIN SCRIPT -------------------------------------------------------------
  # define windows and check argument values
  if (overlap == 0) width = epoch*sf
  if (overlap != 0 & overlap < 1) width = epoch*sf/(1/overlap)
  if (overlap >= 1) stop("\n\nERROR: overlap value must be less than 1\n\n")
  if (!is.na(upperBound)) {
    if (upperBound > (sf/2)) stop("ERROR: upperBound bound must be =<50% of sampling frequency")
  }
  # define feature names
  fnames = c()
  # calculate features
  td = slide(vm, epoch*sf, FUN = time.domain, by = width)
  fnames = c(fnames, colnames(td))
  fd = slide(vm, epoch*sf, FUN = function(x) freq.domain(x, sf = sf, lowerBound, upperBound), by = width)
  fnames = c(fnames, colnames(fd))
  xy = cross.corr(data[,1], data[,2], epoch*sf, by = width)
  xz = cross.corr(data[,1], data[,3], epoch*sf, by = width)
  yz = cross.corr(data[,2], data[,3], epoch*sf, by = width)
  fnames = c(fnames, "xy", "xz", "yz")
  # return features
  features = cbind(td, fd, xy, xz, yz)
  colnames(features) = fnames
  rownames(features) = 1:nrow(features)
  return(features)
}
