#' Aggregate Per Date
#'
#' @description
#' Generates day-level summary data frame
#'
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
#' Adult women Hip RF Ellis)
#' @param classes Character (default = NULL) indicating the behavioural classes estimated by the classifier
#' @param boutdur Numeric vector (default = c(1, 10, 30)) indicating the bout durations over which calculate bouts of behaviors
#' @param boutcriter Numeric (default = 0.8) indicating the proportion of the bout duration that should be classified in a given behavior to consider a bout
#' @param tsDir Directory with time series files.
#' @param visualreport Logical indicating whether to visualize the time series
#' @param boutmaxgap Integer (default = 1) with maximum consecutive gap length allowed in bout calculation.
#'
#' @return Data frame with aggregates of time spent in classes per calendar date.
#'
#' @importFrom grDevices pdf dev.off
#'
#' @author Jairo H. Migueles <jairo@jhmigueles.com>
#' @export
aggregate_per_date = function(tsDir, epoch, classifier, classes,
                              boutdur, boutcriter, boutmaxgap = boutmaxgap,
                              visualreport = FALSE) {
  # readjust classes
  if ("nighttime" %in% classes) {
    classes = gsub("nighttime", "nighttime.awake", classes)
    classes = gsub("sleep", "nighttime.sleep", classes)
  }
  files = dir(tsDir, full.names = TRUE)
  for (fi in 1:length(files)) {
    ts = NULL
    load(files[fi])
    # format timestamp to ease calculations
    ts$timestamp = strptime(paste(ts$date, ts$time), format = "%Y-%m-%d %H:%M:%OS", tz = "")
    ts$time = format(ts$timestamp, "%H:%M:%S")
    if (is.numeric(ts$activity)) { # ensures compatibility with versions <0.1.4
      ts$activity = factor(ts$activity, levels = 1:length(classes), labels = classes)
    }
    # Day of the week, used for summarizing per day
    availableDates = unique(ts$date)
    # matrix to store the results
    ds = as.data.frame(matrix(NA, length(availableDates), 100))
    # store general information -----
    ci = 1
    ds[, ci] = unique(ts$subject)
    dsnames = "ID"; ci = ci + 1
    ds[, ci] = classifier
    dsnames[ci] = "classifier"; ci = ci + 1
    ds[, ci] = availableDates
    dsnames[ci] = "date"; ci = ci + 1
    ds[, ci] = weekdays(as.Date(availableDates), abbreviate = FALSE)
    dsnames[ci] = "weekday"; ci = ci + 1
    ds[, ci] = ifelse(ds[, ci - 1] %in% c("Saturday", "Sunday"), 1, 0)
    dsnames[ci] = "is_weekend"; ci = ci + 1

    # aggregate per dates -------------
    min_in_class = function(x, epoch) table(x)*epoch/60

    # windows duration
    # full day
    fullday = aggregate(!is.na(activity) ~ date, data = ts, FUN = sum)
    rows2fill = which(availableDates %in% fullday$date)
    ds[rows2fill, ci] = fullday[, 2]*epoch/60
    dsnames[ci] = paste("dur", "total", "fullday", "min", sep = "_")
    ci = ci + 1
    #
    # awake & nighttime
    if ("nighttime.awake" %in% classes) {
      # awake
      awake = aggregate(!grepl("nighttime", activity) ~ date, data = ts, FUN = sum)
      rows2fill = which(availableDates %in% awake$date)
      ds[rows2fill, ci] = awake[, 2]*epoch/60
      dsnames[ci] = paste("dur", "total", "awake", "min", sep = "_")
      ci = ci + 1
      # nighttime
      nighttime = aggregate(grepl("nighttime", activity) ~ date, data = ts, FUN = sum)
      rows2fill = which(availableDates %in% nighttime$date)
      ds[rows2fill, ci] = nighttime[, 2]*epoch/60
      dsnames[ci] = paste("dur", "total", "nighttime", "min", sep = "_")
      ci = ci + 1
    }
    # total minutes in classes
    ci2 = ci + length(classes) - 1
    time_in_classes = aggregate(activity ~ date, data = ts, FUN = min_in_class, epoch = epoch)
    rows2fill = which(availableDates %in% time_in_classes$date)
    ds[rows2fill, ci:ci2] = time_in_classes[, 2]
    dsnames[ci:ci2] = paste("dur", "total", classes, "min", sep = "_")
    ci = ci2 + 1

    # start and end of nighttime
    find_start_end = function(ts, column, class) {
      starts = which(diff(c(0, ts[, column] %in% class)) == 1)
      ends = which(diff(c(0, ts[, column] %in% class)) == -1)
      return(list(starts = starts, ends = ends))
    }
    if ("nighttime.awake" %in% classes) {
      noons = which(ts$time == "12:00:00")
      if (sum(grepl("nighttime", ts$activity)) > 0) {
        # if sleep periods have been detected...
        start_end_nighttime = find_start_end(ts, column = "activity",
                                             class = c("nighttime.awake", "nighttime.sleep"))
        start_end_nighttime_dates = NULL
        for (ni in 1:length(start_end_nighttime$ends)) {
          next_noon = which(noons > start_end_nighttime$ends[ni])[1]
          if (is.na(next_noon)) {
            # if there is not a next_noon, meaning that recording finished before 12pm
            # following the last wake up
            prev_noon = max(which(noons < start_end_nighttime$ends[ni]))
            start_end_nighttime_dates[ni] = as.character(as.Date(ts$date[noons[prev_noon]]) + 1)
          } else {
            start_end_nighttime_dates[ni] = ts$date[noons[next_noon]]
          }
        }
        # start_end_nighttime_dates = ts$date[start_end_nighttime$ends] # dates based on wakeup
        rows2fill = which(availableDates %in% start_end_nighttime_dates)
        ds[rows2fill, ci] = as.character(ts$timestamp[start_end_nighttime$starts])
        ds[rows2fill, ci + 1] = as.character(ts$timestamp[start_end_nighttime$ends])
        dsnames[ci:(ci + 1)] = paste("timestamp", c("sleepOnset", "wakeup"), sep = "_")
        ci = ci + 2
      } else {
        # sleep periods have not been detected (e.g., participant removed devices all nights)
        # only store names to be consistent in columns in the full dataset,
        # but leave all data as NA
        dsnames[ci:(ci + 1)] = paste("timestamp", c("sleepOnset", "wakeup"), sep = "_")
        ci = ci + 2
      }
    }
    # bouts of behaviors
    boutdur = sort(boutdur, decreasing = TRUE)
    for (classi in classes) {
      if (grepl("^nighttime|^nonwear", classi)) break
      for (boutduri in 1:length(boutdur)) {
        look4bouts = ifelse(ts$activity == classi, 1, 0)
        # getBout is a copy of GGIR::g.getbout with which we are experimenting
        # to obtain slightly different version of bouts, e.g., allowing for
        # relative and absolute boutcriter
        bouts = getBout(x = look4bouts, boutduration = boutdur[boutduri]*(60/epoch),
                        boutcriter = boutcriter, epoch = epoch, boutmaxgap = boutmaxgap)
        timeInBouts = ifelse(look4bouts == 1 & bouts == 1, 1, 0)
        # bout indicators
        bout_min = aggregate(timeInBouts ~ ts$date, FUN = sum)
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
        # store in ds
        # duration
        rows2fill = which(availableDates %in% bout_min[, 1])
        ds[rows2fill, ci] = round(bout_min[, 2]*epoch/60, 3)
        dsnames[ci] = paste("dur", "day", "bouts", classi, boutdur[boutduri], "min", sep = "_")
        ci = ci + 1
        # count
        rows2fill = which(availableDates %in% bout_count[, 1])
        ds[rows2fill, ci] = bout_count[, 2]
        dsnames[ci] = paste("Nr", "day", "bouts", classi, boutdur[boutduri], sep = "_")
        ci = ci + 1
        # longest duration
        rows2fill = which(availableDates %in% bout_count[, 1])
        ds[rows2fill, ci] = round(bout_longest_min[, 2]*epoch/60, 3)
        dsnames[ci] = paste("dur", "day", "LongestBout", classi, boutdur[boutduri], sep = "_")
        ci = ci + 1
        # shortest duration
        rows2fill = which(availableDates %in% bout_count[, 1])
        ds[rows2fill, ci] = round(bout_shortest_min[, 2]*epoch/60, 3)
        dsnames[ci] = paste("dur", "day", "ShortestBout", classi, boutdur[boutduri], sep = "_")
        ci = ci + 1
        # average duration
        rows2fill = which(availableDates %in% bout_count[, 1])
        ds[rows2fill, ci] = round(bout_avg_min[, 2]*epoch/60, 3)
        dsnames[ci] = paste("dur", "day", "AvgBout", classi, boutdur[boutduri], sep = "_")
        ci = ci + 1
      }
    }
    # visualize if required
    if (visualreport == TRUE) {
      fnDir = gsub("time_series", "results", tsDir)
      fnDir = file.path(fnDir, "visualizations")
      suppressWarnings(dir.create(fnDir, recursive = TRUE))
      grDevices::pdf(file.path(fnDir, paste0(unique(ts$subject), ".pdf")),
                     paper = "a4", width = 0, height = 0)
      actimetricPlot(ts = ts, daysummary = ds, dsnames = dsnames,
                     classes = classes, epoch = epoch)
      grDevices::dev.off()
    }
    # store information in daysummary and next file
    if (fi == 1) daysummary = ds else daysummary = rbind(daysummary, ds)
  }

  # keep only calculated columns and insert colnames
  daysummary = daysummary[, 1:(ci - 1)]
  colnames(daysummary) = dsnames
  # return
  return(daysummary)
}
