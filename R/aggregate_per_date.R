#' Aggregate Per Date
#'
#' @param ts Data frame with ts object from \link{extractFeatures}
#' @param epoch Numeric with the epoch length in seconds.
#' @param classifier Character (default = NULL) indicating the classifier to be used
#' (available options are:
#' Preschool Wrist Random Forest Free Living,
#' Preschool Hip Random Forest Free Living,
#' Preschool Hip Random Forest Free Living Lag-Lead,
#' Preschool Wrist Random Forest Free Living Lag-Lead,
#' School age Wrist Random Forest, School age Hip Random Forest,
#' Adult Wrist RF Trost,
#' Adult women Wrist RF Ellis,
#' Adult women Hip RF Ellis,
#' Thigh Decision Tree)
#' @param classes Character (default = NULL) indicating the behavioural classes estimated by the classifier
#' @param boutdur Numeric vector (default = c(1, 10, 30)) indicating the bout durations over which calculate bouts of behaviors
#' @param boutcriter Numeric (default = 0.8) indicating the proportion of the bout duration that should be classified in a given behavior to consider a bout
#'
#' @return
#'
#' @importFrom GGIR g.getbout
#'
#' @export
aggregate_per_date = function(ts, epoch, classifier, classes,
                    boutdur, boutcriter) {
  # format timestamp to ease calculations
  ts$timestamp = strptime(paste(ts$date, ts$time), format = "%Y-%m-%d %H:%M:%OS", tz = "")
  ts$time = format(ts$timestamp, "%H:%M:%S")
  ts$activity = factor(ts$activity, levels = 1:length(classes), labels = classes)
  # Day of the week, used for summarizing per day
  availableDates = unique(ts$date)
  # matrix to store the results
  daysummary = as.data.frame(matrix(NA, length(availableDates), 100))
  # store general information -----
  ci = 1
  daysummary[, ci] = unique(ts$subject)
  dsnames = "ID"; ci = ci + 1
  daysummary[, ci] = classifier
  dsnames = "classifier"; ci = ci + 1
  daysummary[, ci] = availableDates
  dsnames[ci] = "date"; ci = ci + 1
  daysummary[, ci] = weekdays(as.Date(availableDates), abbreviate = FALSE)
  dsnames[ci] = "weekday"; ci = ci + 1
  daysummary[, ci] = ifelse(daysummary[, ci - 1] %in% c("Saturday", "Sunday"), 1, 0)
  dsnames[ci] = "is_weekend"; ci = ci + 1

  # aggregate per dates -------------
  min_in_class = function(x, epoch) table(x)*epoch/60

  # total minutes in classes
  ci2 = ci + length(classes) - 1
  time_in_classes = aggregate(activity ~ date, data = ts, FUN = min_in_class, epoch = epoch)
  rows2fill = which(availableDates %in% time_in_classes$date)
  daysummary[rows2fill, ci:ci2] = time_in_classes[, 2]
  dsnames[ci:ci2] = paste("dur", "day", "total", "class", classes, "min", sep = "_")
  ci = ci2 + 1

  # start and end of nighttime
  find_start_end = function(ts, column, class) {
    starts = which(diff(c(0, ts[, column] == class)) == 1)
    ends = which(diff(c(0, ts[, column] == class)) == -1)
    return(list(starts = starts, ends = ends))
  }
  start_end_nighttime = find_start_end(ts, column = "activity", class = "nighttime")
  start_end_nighttime_dates = ts$date[start_end_nighttime$ends] # dates based on wakeup
  rows2fill = which(availableDates %in% start_end_nighttime_dates)
  daysummary[rows2fill, ci] = as.character(ts$timestamp[start_end_nighttime$starts])
  daysummary[rows2fill, ci + 1] = as.character(ts$timestamp[start_end_nighttime$ends])
  dsnames[ci:(ci + 1)] = paste("timestamp", c("sleepOnset", "wakeup"), sep = "_")
  ci = ci + 2

  # sleep time during nighttime
  time_in_sleep = aggregate(sleep_periods == 1 ~ date, data = ts, FUN = sum)
  rows2fill = which(availableDates %in% time_in_sleep$date)
  daysummary[rows2fill, ci] = time_in_sleep[,2]*epoch/60
  dsnames[ci] = paste("dur", "nighttime", "totalSleep", "min", sep = "_")
  ci = ci + 1

  # sleep status during nighttime
  sleep_status = cut(ts$enmo*1000, breaks = c(-Inf, 63.3, Inf), right = FALSE,
                     labels = c("l", "r"))
  sleep_status = factor(sleep_status, levels = c("l", "r", "m"))
  post = which(ts$vm.sd > 0.013)
  q1 = which(diff(post) > ((60/epoch)*5))
  if (length(q1) > 0) {
    for (iii in 1:length(q1)) {
      sleep_status[post[q1[iii]]:post[q1[iii] + 1]] = "m"
    }
  }
  sleep_status[which(ts$activity != "nighttime")] = NA
  ci2 = ci + 2
  time_in_sleepStatus = aggregate(sleep_status ~ ts$date,
                                  FUN = min_in_class, epoch = epoch)
  rows2fill = which(availableDates %in% start_end_nighttime_dates)
  daysummary[rows2fill, ci:ci2] = time_in_sleepStatus[, 2]
  dsnames[ci:ci2] = paste("dur", "nighttime", c("motionless", "lightSleep", "restless"), "min", sep = "_")
  ci = ci2 + 1

  # mean acceleration per class
  if ("enmo" %in% colnames(ts)) {
    for (classi in classes) {
      acc_in_class = tryCatch(aggregate(enmo ~ date, data = ts, FUN = mean,
                                        subset = activity == classi),
                              error = function(e) matrix())
      if (ncol(acc_in_class) > 1) { # if no time is classified in a given activity in the whole recording, then ncol == 1
        rows2fill = which(availableDates %in% acc_in_class$date)
        daysummary[rows2fill, ci] = round(acc_in_class[, 2]*1000, 3)
      }
      dsnames[ci] = paste("ENMO.mean", "total", "class", classi, "mg", sep = "_")
      ci = ci + 1
    }
  }
  if ("vm.mean" %in% colnames(ts)) {
    for (classi in classes) {
      acc_in_class = tryCatch(aggregate(enmo ~ date, data = ts, FUN = mean,
                                        subset = activity == classi),
                              error = function(e) matrix())
      if (ncol(acc_in_class) > 1) { # if no time is classified in a given activity in the whole recording, then ncol == 1
        rows2fill = which(availableDates %in% acc_in_class$date)
        daysummary[rows2fill, ci] = round(acc_in_class[, 2]*1000, 3)
      }
      dsnames[ci] = paste("VM.mean", "total", "class", classi, "mg", sep = "_")
      ci = ci + 1
    }
  }
  if (any(grepl("agcounts", colnames(ts)))) {
    for (classi in classes) {
      for (axis in c("x", "y", "z", "vm")) {
        ax = paste("agcounts", axis, sep = "_")
        acc_in_class = tryCatch(aggregate(ts[, ax] ~ ts$date, FUN = mean,
                                          subset = ts$activity == classi),
                                error = function(e) matrix())
        if (ncol(acc_in_class) > 1) { # if no time is classified in a given activity in the whole recording, then ncol == 1
          rows2fill = which(availableDates %in% acc_in_class[,1])
          daysummary[rows2fill, ci] = round(acc_in_class[, 2], 3)
        }
        dsnames[ci] = paste(paste0("countsPer", epoch, "s"), "total", "class", classi,
                            sep = "_")
        ci = ci + 1
      }
    }
  }
  if (any(grepl("LFEcounts", colnames(ts)))) {
    for (classi in classes) {
      for (axis in c("x", "y", "z", "vm")) {
        ax = paste("LFEcounts", axis, sep = "_")
        acc_in_class = tryCatch(aggregate(ts[, ax] ~ ts$date, FUN = mean,
                                          subset = ts$activity == classi),
                                error = function(e) matrix())
        if (ncol(acc_in_class) > 1) { # if no time is classified in a given activity in the whole recording, then ncol == 1
          rows2fill = which(availableDates %in% acc_in_class[,1])
          daysummary[rows2fill, ci] = round(acc_in_class[, 2], 3)
        }
        dsnames[ci] = paste(paste0("LFEcountsPer", epoch, "s"), "total", "class", classi,
                            sep = "_")
        ci = ci + 1
      }
    }
  }

  # bouts of behaviors
  boutdur = sort(boutdur, decreasing = TRUE)
  for (classi in classes) {
    if (classi %in% c("nighttime", "nonwear")) break
    for (boutduri in 1:length(boutdur)) {
      look4bouts = ifelse(ts$activity == classi, 1, 0)
      bouts = GGIR::g.getbout(x = look4bouts, boutduration = boutdur[boutduri]*(60/epoch),
                              boutcriter = boutcriter, ws3 = epoch)
      # bout indicators
      bout_min = aggregate(bouts ~ ts$date, FUN = sum)
      bout_count = aggregate(bouts ~ ts$date, FUN = function(x) sum(rle(x)$values))
      bout_longest_min = aggregate(bouts ~ ts$date,
                                   FUN = function(x) {
                                     rle = rle(x)
                                     what = which(rle$values == 1)
                                     if (length(what) > 0 ) max(rle$lengths[what]) else NA
                                     })
      bout_shortest_min = aggregate(bouts ~ ts$date,
                                    FUN = function(x) {
                                      rle = rle(x)
                                      what = which(rle$values == 1)
                                      if (length(what) > 0 ) min(rle$lengths[what]) else NA
                                    })
      bout_avg_min = aggregate(bouts ~ ts$date,
                               FUN = function(x) {
                                 rle = rle(x)
                                 what = which(rle$values == 1)
                                 if (length(what) > 0 ) mean(rle$lengths[what]) else NA
                               })
      # store in daysummary
      # duration
      rows2fill = which(availableDates %in% bout_min[, 1])
      daysummary[rows2fill, ci] = round(bout_min[, 2]*epoch/60, 3)
      dsnames[ci] = paste("dur", "day", "bouts", classi, boutdur[boutduri], "min", sep = "_")
      ci = ci + 1
      # count
      rows2fill = which(availableDates %in% bout_count[, 1])
      daysummary[rows2fill, ci] = bout_count[, 2]
      dsnames[ci] = paste("Nr", "day", "bouts", classi, boutdur[boutduri], sep = "_")
      ci = ci + 1
      # longest duration
      rows2fill = which(availableDates %in% bout_count[, 1])
      daysummary[rows2fill, ci] = round(bout_longest_min[, 2]*epoch/60, 3)
      dsnames[ci] = paste("dur", "day", "LongestBout", classi, boutdur[boutduri], sep = "_")
      ci = ci + 1
      # shortest duration
      rows2fill = which(availableDates %in% bout_count[, 1])
      daysummary[rows2fill, ci] = round(bout_shortest_min[, 2]*epoch/60, 3)
      dsnames[ci] = paste("dur", "day", "ShortestBout", classi, boutdur[boutduri], sep = "_")
      ci = ci + 1
      # average duration
      rows2fill = which(availableDates %in% bout_count[, 1])
      daysummary[rows2fill, ci] = round(bout_avg_min[, 2]*epoch/60, 3)
      dsnames[ci] = paste("dur", "day", "AvgBout", classi, boutdur[boutduri], sep = "_")
      ci = ci + 1
    }
  }
  # keep only calculated columns and insert colnames
  daysummary = daysummary[, 1:(ci - 1)]
  colnames(daysummary) = dsnames
  return(daysummary)
}
