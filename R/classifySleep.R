#' Classifies nighttime and sleep in the time series
#'
#' @description
#' Function to classify nighttime and sleep in the time series.
#'
#' @param anglez Angle for the z axis relative to the horizontal plane.
#' @param starttime Start time as exported from \link{ReadAndCalibrate}
#' @param classifier Character (default = NULL) indicating the classifier to be used
#' (available options are:
#' Preschool Wrist Random Forest Free Living,
#' Preschool Hip Random Forest Free Living,
#' Preschool Hip Random Forest Free Living Lag-Lead,
#' Preschool Wrist Random Forest Free Living Lag-Lead,
#' School age Wrist Random Forest, School age Hip Random Forest,
#' Adult Wrist RF Trost,
#' Adult women Wrist RF Ellis,
#' Adult women Hip RF Ellis)
#' @param ts Data frame with time series.
#' @param infoClassifier Information relative to the selected classifier as exported from \link{GetInfoClassifier}
#' @param do.sleep Logical (default = TRUE) indicating if sleep should be calculated
#' @param do.nonwear Logical (default = TRUE) indicating if nonwear should be calculated
#'
#' @return Numeric vector with classified activities including nighttime and sleep.
#' @export
#' @references GGIR R Package. doi: 10.1123/jmpb.2018-0063
#' @references Ahmadi MN, Nathan N, Sutherland R, Wolfenden L, Trost SG. Non-wear
#' or sleep? Evaluation of five non-wear detection algorithms for raw accelerometer
#' data. J Sports Sci. 2020 Feb;38(4):399-404. doi: 10.1080/02640414.2019.1703301
#' @author Matthew N. Ahmadi <matthew.ahmadi@sydney.edu.au>
classifySleep = function(anglez, starttime, classifier, infoClassifier, ts, do.sleep, do.nonwear) {
  # original code provided by Matthew N. Ahmadi
  # minor clean up conducted by Jairo H. Migueles
  epoch = infoClassifier$epoch
  classes = infoClassifier$classes
  # DETECT SLEEP -----------------------
  # Using variability of angle z as in GGIR.
  if (do.sleep == TRUE) {
    # derive timestamp for anglez
    ts_anglez = deriveTimestamps(from = starttime, length = length(anglez), epoch = 5)
    anglez = data.frame(date = ts_anglez[, 1], time = ts_anglez[, 2], anglez = anglez)
    # get classes information
    ts$sleep_windows_orig = ts$sleep_periods = ts$nighttime = 0
    nighttime_id = length(classes) + 1
    sleep_id = length(classes) + 2
    classes = c(classes, "nighttime", "sleep")
    # 1 - detect nighttime chunks
    availableDates = unique(anglez$date)
    b = 1; c = 2
    for (sss in 1:length(availableDates)) {
      if (c > length(availableDates)) { # end reached
        break
      }
      try(
        if (c <= length(availableDates)) {
          di = availableDates[b]
          nextDi = availableDates[c]
          from = which(anglez$date == di & anglez$time >= "12:00:00")[1]
          to = max(which(anglez$date == nextDi & anglez$time <= "12:00:00"))
          if (length(to) == 0) break
          to = to - 1
          ga = anglez[from:to, ]
          ga = ga[!is.na(ga$anglez), ] #For last day when monitor is plugged in and acc is 0's
          sleepw = inbed(angle = ga$anglez, outofbedsize = 30, ws3 = 5, bedblocksize = 30, k = 60)
          if (sleepw$lightsout[1] == 0) sleepw$lightsout[1] = 1
          d1 = ga$date[sleepw$lightsout]
          t1 = ga$time[sleepw$lightsout]
          e = which(ts$date %in% d1 & ts$time >= t1)[1]
          d1 = ga$date[sleepw$lightson]
          t1 = ga$time[sleepw$lightson]
          e1 = max(which(ts$date %in% d1 & ts$time <= t1))
          for (zz in 1:length(e)) ts$nighttime[e[zz]:e1[zz]] = 1
        }, silent = T)
      # next night
      b = b + 1
      c = c + 1
    }
  }
  # add nonwear to ts
  ts$activity = as.numeric(ts$activity)
  nonwear_id = length(classes) + 1
  classes = c(classes, "nonwear")
  ts$nonwear_orig = ts$nonwear
  e = which(ts$nonwear == 1)
  if (length(e) > 0) ts$activity[e] = nonwear_id
  # adjust sleep and nonwear based on preset 75% Decision Fusion Rule ----
  if (do.sleep == TRUE) {
    hold = diff(c(0, ts$nighttime))
    st = which(hold == 1); end = which(hold == -1) - 1
    if (length(st) == 0) st = 1
    if (length(st) > length(end)) end = c(end, length(hold))
    for (nw in 1:length(st)) {
      nw_duringSleep = length(which(ts$nonwear[st[nw]:end[nw]] == 1))
      sl = sum(ts$nighttime[st[nw]:end[nw]])
      if (!is.na(nw_duringSleep/sl)) {
        if (nw_duringSleep/sl < 0.75) ts$nonwear[st[nw]:end[nw]] = 2 #re-classify nonwear as nighttime
      }
    }
    # nonwear recategorized to nighttime
    e = which(ts$nonwear == 2)
    if (length(e) > 0) ts$activity[e] = nighttime_id
    # DETECT SLEEP WITHIN THE NIGHTTIME
    ts$sleep_windows_orig = ts$nighttime
    ts$sleep_periods = rep(0,nrow(ts))
    if (any(ts$activity == nighttime_id)) {
      # make all instances into binary sleep 1/0
      sleep = ifelse(ts$activity == nighttime_id, 1, 0)
      sleep = diff(c(0, sleep))
      # identify start and end of each sleep window
      start = which(sleep == 1); end = which(sleep == -1)
      #in case monitoring period ends during a sleep window
      if (!length(end) == length(start)) end = c(end,nrow(ts))
      # loop through all sleep windows
      for (i in 1:length(start)) {
        tilt = ts$tilt[start[i]:end[i]]
        sdl1 = rep(0,length(tilt))
        postch = abs(diff(tilt))
        postch = ifelse(postch <= 5, 1, 0)
        postch = c(postch, postch[length(postch)]) # repeat last instance to make the length of "postch" the same as "tilt"
        run = rle(postch)
        run = rep(run$lengths,run$lengths)
        postch = cbind(postch,run)
        e = which(postch[,1] == 1 & postch[,2] >= (5*(60/epoch)))
        if (length(e) > 0) sdl1[e] = 1
        ts$sleep_periods[start[i]:end[i]] = sdl1
      }
    }
    # recode activity with sleep within nighttime
    if (sum(ts$sleep_periods == 1) > 0) {
      ts$activity[which(ts$sleep_periods == 1)] = sleep_id
    }
  }
  # recode nighttime as nighttime.awake and sleep as nighttime.sleep
  classes = gsub("nighttime", "nighttime.awake", classes)
  classes = gsub("sleep", "nighttime.sleep", classes)
  activity = factor(x = ts$activity, levels = 1:length(classes), labels = classes)
  return(activity)
}
