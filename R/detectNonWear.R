#' Detects Non Wear Periods
#'
#' @description
#' Function to detect non-wear periods based on the SD of the vector magnitude
#' of the raw accelerations. In brief, it looks for 30-min windows in which the
#' SD of the vector magnitude is lower than a given threshold (default = 0.013 G
#' units). Additionally, if short non-wear periods (max 30 min) are surrounded with
#' at least 30% of their duration by non-wear periods, then these short wear periods
#' are converted to non-wear (additional non-wear).
#'
#' @param data Matrix with 3 columns containing the raw acceleration for X, Y,
#' and Z axes in G units.
#' @param sf Numeric indicating the sampling frequency in Hertz.
#' @param epoch Numeric with the epoch length in seconds.
#' @param sdThreshold Threshold to define nonwear time in g units (default 13 mgs).
#'
#' @return Numeric vector of length = nrow(data) indicating wear (0) and nonwear (1).
#' @export
#' @references GGIR R Package. doi: 10.1123/jmpb.2018-0063
#' @references Ahmadi MN, Nathan N, Sutherland R, Wolfenden L, Trost SG. Non-wear
#' or sleep? Evaluation of five non-wear detection algorithms for raw accelerometer
#' data. J Sports Sci. 2020 Feb;38(4):399-404. doi: 10.1080/02640414.2019.1703301
#' @author Matthew N. Ahmadi <matthew.ahmadi@sydney.edu.au>
detectNonWear = function(data, sf, epoch, sdThreshold = 13/1000) {
  # original code provided by Matthew N. Ahmadi
  # look for windows with sd lower than sdThreshold, similar to GGIR.
  # vector magnitude
  vm = sqrt(rowSums(data[, 1:3]^2))
  # 1 - NONWEAR BASED ON SD THRESHOLD -----------
  # sd crit
  vm = as.numeric(slide(x = vm, width = epoch*sf, FUN = sd))
  vm2 = ifelse(test = vm < sdThreshold, yes = 1, no = 0)
  # Duration (in epochs) of periods below SD threshold
  run = rle(vm2)
  run = rep(run$lengths, run$lengths)
  vm2 = cbind(vm2, run)
  # SD below threshold for at least 30 minutes = nonwear
  nonwear = rep(0, length(vm))
  epochs_in_30min = 60/epoch*30
  e = which(vm2[ ,1] == 1 & vm2[,2] >= epochs_in_30min)
  if (length(e) > 0) nonwear[e] = 1
  # 2 - ADDITIONAL NONWEAR ------------------
  # if wear surrounded of nonwear -> nonwear
  check = nonwear
  run = rle(check)
  run = as.matrix(list2DF(run))
  # Create a table with row length equal to wear periods
  if (any(run[,2] == 0)) {
    wear = matrix(0, length(which(run[,2] == 0)), 3,
                  dimnames = list(1:length(which(run[,2] == 0)),
                                  c("before", "current", "after")))
    i = 1; ii = 1
    while (i <= nrow(run)) {
      if (run[i, 2] == 0) {
        # for each wear period...
        if (i > 1) wear[ii, 1] = run[i - 1, 1] # before period
        wear[ii, 2] = run[i, 1] # current period
        if (i < nrow(run)) wear[ii, 3] = run[i + 1, 1] # after period
        # if wear period < 30 min and less than 30% of bordering nonwear, convert to nonwear
        nwBordering = (wear[ii,2]/(wear[ii, 1] + wear[ii, 3]))
        if (wear[ii, 2] < epochs_in_30min & nwBordering < 0.3) run[i,2] = 1
        # next iteration
        ii = ii + 1
      }
      i = i + 1
    }
    check = rep(run[,2], run[,1])
    # sum original nonwear plus additional nonwear
    nonwear = nonwear + check
    # any nonwear >= 1 is nonwear and make all values 1 to make binary outcome; 0/1
    nonwear[nonwear >= 1] = 1
  }
  return(nonwear)
}
