#' Calculates inbed times.
#'
#' @param angle Numeric with angle calculated.
#' @param k Interval for rolling window.
#' @param perc Percentage to establish threshold for inbed time definition.
#' @param inbedthreshold Threshold for inbed time definition.
#' @param bedblocksize Minimum time interval to define a bed block.
#' @param outofbedsize Maximum gap allowed in bed blocks.
#' @param ws3 Epoch size.
#'
#' @return List with identificators of inbed time.
#' @export
#' @references GGIR R Package. doi: 10.1123/jmpb.2018-0063
#' @references van Hees VT, Sabia S, Jones SE, Wood AR, Anderson KN, Kivimäki M,
#' Frayling TM, Pack AI, Bucan M, Trenell MI, Mazzotti DR, Gehrman PR, Singh-Manoux BA,
#' Weedon MN. Estimating sleep parameters using an accelerometer without sleep diary.
#' Sci Rep. 2018 Aug 28;8(1):12975. doi: 10.1038/s41598-018-31266-z.
#' @references Ahmadi MN, Nathan N, Sutherland R, Wolfenden L, Trost SG. Non-wear
#' or sleep? Evaluation of five non-wear detection algorithms for raw accelerometer
#' data. J Sports Sci. 2020 Feb;38(4):399-404. doi: 10.1080/02640414.2019.1703301
#' @author Matthew N. Ahmadi <matthew.ahmadi@sydney.edu.au>
inbed = function(angle, k = 60, perc = 0.1, inbedthreshold = 15,
                 bedblocksize = 30, outofbedsize = 60, ws3 = 5) {
  medabsdi = function(angle) {
    angvar = stats::median(abs(diff(angle)))
    return(angvar)
  }
  x = slide(angle, width = k, FUN = medabsdi, by = 1)
  nomov = rep(0, length(x))
  inbedtime = rep(NA, length(x))
  pp = quantile(x, probs = c(perc)) * inbedthreshold
  if (pp == 0)
    pp = .2
  nomov[which(x < pp)] = 1
  nomov = c(0, nomov, 0)
  s1 = which(diff(nomov) == 1)
  e1 = which(diff(nomov) == -1)
  bedblock = which((e1 - s1) > ((60/ws3) * bedblocksize * 1))
  if (length(bedblock) > 0) {
    s2 = s1[bedblock]
    e2 = e1[bedblock]
    for (j in 1:length(s2)) {
      inbedtime[s2[j]:e2[j]] = 1
    }
    outofbed = rep(0, length(inbedtime))
    outofbed[which(is.na(inbedtime) == TRUE)] = 1
    outofbed = c(0, outofbed, 0)
    s3 = which(diff(outofbed) == 1)
    e3 = which(diff(outofbed) == -1)
    outofbedblock = which((e3 - s3) < ((60/ws3) * outofbedsize *
                                         1))
    if (length(outofbedblock) > 0) {
      s4 = s3[outofbedblock]
      e4 = e3[outofbedblock]
      if (length(s4) > 0) {
        for (j in 1:length(s4)) {
          inbedtime[s4[j]:e4[j]] = 1
        }
      }
    }
    if (length(inbedtime) == (length(x) + 1))
      inbedtime = inbedtime[1:(length(inbedtime) -
                                 1)]
    inbedtime2 = rep(1, length(inbedtime))
    inbedtime2[which(is.na(inbedtime) == TRUE)] = 0
    s5 = which(diff(c(0, inbedtime2, 0)) == 1)
    e5 = which(diff(c(0, inbedtime2, 0)) == -1)
    inbeddurations = e5 - s5
    longestinbed = which(inbeddurations == max(inbeddurations))
    lightsout = s5[longestinbed] - 1
    lightson = e5[longestinbed] - 1
    if (length(s5) > 1) {
    naplightsout <- s5[-longestinbed] - 1
    naplightson <- e5[-longestinbed] - 1
    } else {
      naplightsout <- NA
      naplightson <- NA
    }
  } else {
    lightson = c()
    lightsout = c()
    tib.threshold = c()
  }
  tib.threshold = pp
  invisible(list(lightsout = lightsout, lightson = lightson,
                 tib.threshold = tib.threshold,naplightsout = naplightsout,
                 naplightson = naplightson))
}

