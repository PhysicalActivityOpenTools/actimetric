#' Calculate bouts of behavior
#' @description
#' Calculates bouts of the different behaviors provided by the classifier
#'
#' @param x Binary numeric vector (default = NULL) with 1's indicating occurrence of behavior.
#' @param boutduration Integer (default = 10) indicating the length of the bout in minutes.
#' @param boutcriter Numeric (default = 0.8). If -1, then no gaps within the bout are allowed.
#' If a number lower than 1, then it is considered as a proportion of the boutduration (e.g., 0.8 means 80%).
#' If 1 or larger, then the gap is interpreted in absolute number of minutes (e.g., 2 means 2 minutes).
#' @param epoch Integer (default = 5) indicating the epoch length in seconds.
#' @param boutmaxgap Integer (default = 1) with maximum consecutive gap length allowed in bout calculation.
#'
#' @return
#' @export
getBout = function(x, boutduration = 10, boutcriter = 0.8, boutmaxgap = 1, epoch = 5) {
  # define max absolute gap
  if (boutcriter >= 1) {
    boutmaxgap = ifelse(test = boutmaxgap < boutcriter, yes = boutcriter, no = boutmaxgap)
  }
  maxGap = boutmaxgap * 60 # turn to seconds
  p = which(x == 1)
  x[is.na(x)] = 0 # ignore NA values in the unlikely event that there are any
  xt = x
  #look for breaks larger than maxGap
  # Note: we do + 1 to make sure we look for breaks larger than but not equal to a minute,
  # this is critical when working with 1 minute epoch data
  # we need to append 0's at the beginning and end so that first and last epochs in timeseries do not get ignored
  zeroes = rep(0, ceiling(60/epoch/2))
  xtmp = c(zeroes, x, zeroes)
  lookforbreaks = zoo::rollmean(x = xtmp, k = (maxGap/epoch) + 1, align = "center", fill = rep(0,3))
  # remove appended zeroes
  keep = (length(zeroes) + 1):(length(lookforbreaks) - length(zeroes))
  #insert negative numbers to prevent these minutes to be counted in bouts
  #in this way there will not be bouts breaks lasting longer than 1 minute
  xtmp[lookforbreaks == 0] = -boutduration
  xt = xtmp[keep]
  # apply boutcriter (if percentage)
  if (boutcriter < 1) {
    # append 0's to define boutcriter
    append = rep(-boutduration, ceiling(boutduration/2))
    xtmp = c(append, xt, append)
    # if -1, then no breaks allowed, the boutcriter = 1 (100%)
    if (boutcriter == -1) boutcriter = 1
    RM = zoo::rollmean(x = xtmp, k = boutduration, align = "center", fill = rep(0,3))
    keep = (length(append) + 1):(length(RM) - length(append))
    RM = RM[keep]
    p = which(RM >= boutcriter)
    half1 = floor(boutduration/2)
    half2 = boutduration - half1
    # now mark all epochs that are covered by the detected bout/s
    detected_bouts = split(p, cumsum(c(1, diff(p) != 1)))
    if (length(detected_bouts) == 1 & length(detected_bouts[[1]]) == 0) {
      # if no bouts are detected
      x[which(xt != 2)] = 0
      x[which(xt == 2)] = 1
      boutcount = x # distinction not made anymore, but object kept to preserve output structure
    } else {
      # if bouts are detected
      for (bout_i in 1:length(detected_bouts)) {
        bout = detected_bouts[[bout_i]]
        # find start of bout
        start_found = FALSE
        adjust = 0
        while (start_found == FALSE) {
          start = min(bout) - half1 + adjust
          if (start < 1) {
            adjust = adjust + 1
            next
          }
          if (xt[start] != 1) {
            # a bout cannot start without meetin threshold crit
            adjust = adjust + 1
            next
          }
          start_period = start:max(bout)
          look4start = split(xt[start_period], cumsum(c(1, diff(xt[start_period]) != 0)))
          max_gap = 60/epoch
          zeros = c()
          for (i in 1:length(look4start)) {
            zeros = c(zeros, sum(look4start[[i]] == 0))
          }
          if (all(zeros <= max_gap)) start_found = TRUE
          adjust = adjust + 1
        }
        # find end of bout
        end_found = FALSE
        adjust = 0
        while (end_found == FALSE) {
          end = max(bout) + half1 - adjust
          if (end > length(xt)) {
            adjust = adjust + 1
            next
          }
          if (xt[end] != 1) {
            # a bout cannot start without meetin threshold crit
            adjust = adjust + 1
            next
          }
          end_period = min(bout):end
          look4end = split(xt[end_period], cumsum(c(1, diff(xt[end_period]) != 0)))
          max_gap = 60/epoch
          zeros = c()
          for (i in 1:length(look4start)) {
            zeros = c(zeros, sum(look4start[[i]] == 0))
          }
          if (all(zeros <= max_gap)) end_found = TRUE
          adjust = adjust + 1
        }
        xt[start:end] = 2
      }
      x[which(xt != 2)] = 0
      x[which(xt == 2)] = 1
    }
  } else {
    # absolute threshold
    # keep only zeroes that are gaps in the bout, remove starting/ending zeroes
    zeroes = which(xt == 0)
    for (z in zeroes) {
      # before
      bf = 0; i = 1
      while (bf == 0) {
        bf = xt[z - i]
        i = i + 1
      }
      # after
      af = 0; i = 1
      while (af == 0) {
        af = xt[z + i]
        i = i + 1
      }
      # check if it is not surrounded by ones, and then turn off zeroes
      if (bf != 1 | af != 1) xt[z] = -boutduration
    }
    x = ifelse(xt == -boutduration, 0, 1)
  }
  # remove x shorter than boutduration
  rleX = rle(x)
  if (any(rleX$values == 1 & rleX$lengths < boutduration)) {
    RM = which(rleX$values == 1 & rleX$lengths < boutduration)
    for (RMi in RM) {
      from = sum(rleX$lengths[1:(RMi - 1)]) + 1
      to = from + rleX$lengths[RMi] - 1
      turnoff = from:to
      x[turnoff] = 0
    }
  }
  return(x)
}
