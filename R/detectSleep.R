#' Detects Sleep Periods
#'
#' @param data Matrix with 3 columns containing the raw acceleration for X, Y,
#' and Z axes in G units.
#' @param ts Data frame with ts object from \link{extractFeatures}
#' @param epoch Numeric with the epoch length in seconds.
#' @param sf Numeric with the sampling frequency in Hz.
#' @param start_time Start time.
#' @param sleep_id Identification number for sleep periods.
#' @param nonwear_id Identification number for nonwear periods.
#'
#' @return
#' @export
detectSleep = function(data, ts, epoch, sf, start_time, sleep_id, nonwear_id) {
  # 1 - get angle z
  prevChunk = 0; lastChunk = FALSE
  while (lastChunk == FALSE) {
    select = chunkIndexing(prevChunk = prevChunk, sf = sf, rawEnd = nrow(data))
    # info for next iteration
    prevChunk = prevChunk + 1; lastChunk = select$lastChunk
    # indices to read in current iteration
    select = select$select
    # z angle variability per 5 seconds (if sleep is required)
    az = (atan(data[select, 3] / (sqrt(data[select, 1]^2 + data[select, 2]^2)))) / (pi/180)
    az = slide(x = az, width = epoch*sf, FUN = mean)
    if (prevChunk == 1) anglez = az else anglez = c(anglez, az)
  }
  # 2 - add timestamp
  ts_anglez = deriveTimestamps(from = start_time, length = length(anglez), epoch = epoch)
  anglez = data.frame(date = ts_anglez[, 1], time = ts_anglez[, 2], anglez = anglez)
  # 3 - detect sleep
  availableDates = unique(anglez$date)
  b = 1
  c = 2
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
        for (zz in 1:length(e)) ts$sleep[e[zz]:e1[zz]] = 1

      }, silent = T)
    # next night
    b = b + 1
    c = c + 1
  }
  ###########adjust sleep and nonwear based on preset 75% Decision Fusion Rule
  ts$nonwear_orig = ts$nonwear
  hold = ts$sleep
  hold = c(0, diff(hold))
  st = which(hold == 1)
  if (length(st) == 0) st = 1
  end = which(hold == -1)
  end = end - 1
  if (length(st) > length(end)) end = c(end,length(hold))
  for (nw in 1:length(st)) {
    nw_duringSleep = length(which(ts$nonwear[st[nw]:end[nw]] == 1))
    sl = sum(ts$sleep[st[nw]:end[nw]])
    if (!is.na(nw_duringSleep/sl)) {
      if (nw_duringSleep/sl < 0.75) ts$nonwear[st[nw]:end[nw]] = 6
    }
  }
  # nonwear recategorized to 6
  e = which(ts$nonwear == 6)
  ts$activity[e] = sleep_id
  e = which(ts$nonwear == 1)
  ts$activity[e] = nonwear_id

  # detect sleep periods
  ts$sleep_windows_orig = ts$sleep
  ts$sleep_periods = detect_sleep_periods(ts, epoch, sleep_id)

  ts = subset(ts, select = -c(nonwear, sleep))
  return(ts)
}
