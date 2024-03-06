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
#'
#' @return Numeric vector of length = nrow(data) indicating wear (0) and nonwear (1).
#' @export
detectNonWear = function(data, sf, epoch, sdThreshold = 13/1000) {
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
  # # return nonwear in sampling frequency resolution
  # nonwear = rep(nonwear, each = epoch*sf)
  # # make sure nonwear is of the same length as nrow(data)
  # if (length(nonwear) < nrow(data)) {
  #   z = abs(as.numeric(length(nonwear) - nrow(data)))
  #   z = rep(nonwear[length(nonwear)], z)
  #   nonwear = c(nonwear,z)
  # } else if (length(nonwear) > nrow(data)) {
  #   nonwear = nonwear[1:nrow(data)]
  # }
  return(nonwear)
}
