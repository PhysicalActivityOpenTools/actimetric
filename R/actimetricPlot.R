#' Generates plots from the time series classified
#'
#' @param ts Time series with the activity classified to be stored.
#' @param daysummary Matrix with the day-level summary for this recording.
#' @param classes Expected classes classified in the time series.
#' @param dsnames Names of the columns in the daysummary matrix.
#'
#' @return Does not return any object, it just draws the plots.
#' @export
actimetricPlot = function(ts, daysummary, dsnames, classes, epoch) {
  # internal functions
  find_start_end = function(ts, column, class) {
    starts = which(diff(c(0, ts[, column] %in% class, 0)) == 1)
    ends = which(diff(c(0, ts[, column] %in% class, 0)) == -1)
    return(list(starts = starts, ends = ends))
  }
  # code -----
  colnames(daysummary) = dsnames
  # layout for plots
  nClasses = length(classes)
  # selection of variables
  totalClasses = paste("dur_total", classes, "min", sep = "_")
  totals = which(dsnames %in% totalClasses)
  colors = c("#FFFFB3", "#BEBADA", "#FB8072", "#B3DE69",
             "#FDB462", "#FCCDE5", "#BC80BD", "#CCEBC5")[1:nClasses]
  if (any(grepl("^nighttime", classes))) {
    colors[which(classes == "nighttime.awake")] = "lightblue"
    colors[which(classes == "nighttime.sleep")] = "lightgrey"
  }
  if (any(grepl("nonwear", classes))) {
    colors[which(classes == "nonwear")] = "dimgrey"
  }
  # Page 1: Information
  id = unique(ts$subject)
  classifier = unique(daysummary[, "classifier"])
  classifier = paste0(toupper(substr(classifier, 1, 1)), substr(classifier, 2, nchar(classifier)))
  meanTotals = apply(daysummary[, totals], MARGIN = 2, FUN = mean, na.rm = TRUE)
  sdTotals = apply(daysummary[, totals], MARGIN = 2, FUN = sd, na.rm = TRUE)
  names = gsub("^dur_total_|_min$", "", names(meanTotals))
  descTotals = paste0(names, ": ",
                      round(meanTotals), " (SD = ",
                      format(round(sdTotals, 1), nsmall = 1), ") min/day")
  par(las = 1,
      omi = c(0, 0, 0.2, 0), mar = c(3, 3, 2, 2) + 0.1)
  plot(1:20, 1:20, type = "n", axes = FALSE)
  title(main = paste0("ID: ", id), cex.main = 2, adj = 0)
  text(0.2, 19.6, font = 3, pos = 4,
       labels = paste0("Note these values come without any cleaning of the data. Incomplete days may\n",
                       "hightly affect the estimates, please find clean data sets in the results folder."))
  text(0.2, 18.6, font = 3, pos = 4,
       labels = paste0("Classifier: ", classifier))

  text(0.2, 17.6:(17.6 - length(descTotals) + 1), labels = descTotals, font = 1, pos = 4)

  # Page 2: totals
  par(mfrow = c(5, 2), las = 1,
      omi = c(0, 0, 0.2, 0), mar = c(3, 3, 2, 2) + 0.1)
  for (classi in 1:nClasses) {
    # stacked barplot for nighttime awake and sleep
    if (grepl("nighttime.sleep", classes[classi])) next
    if (grepl("nighttime.awake", classes[classi])) {
      heights = t(as.matrix(daysummary[,c(totals[classi], totals[classi + 1])]))[2:1,]
      color = c("lightblue", "lightgrey")
    } else {
      heights = t(as.matrix(daysummary[,totals[classi]]))
      if (classes[classi] != "nonwear") color = colors[classi] else color = "dimgrey"
    }
    # plot options
    xnames = paste0(weekdays(as.Date(daysummary[, "date"]), abbreviate = T),
                    "\n", daysummary[, "date"])
    ylim = c(0, ceiling(max(colSums(heights))*1.3))
    if (ylim[2] == 0) ylim[2] = 60
    # plot
    b = barplot(height = heights, names.arg = xnames,
                col = color, ylim = ylim, cex.names = 0.9,
                main = gsub(".awake$", "", classes[classi]))
    abline(h = 0)
    text(x = b, y = colSums(heights), pos = 3, labels = round(colSums(heights)))
  }
  # plot 2: time series
  layout(matrix(c(1,1,2,3,3,4,5,5,6,7,7,8,9,9,10,11,11,12,13,13,14),
                nrow = 7*3, ncol = 1))
  par(mar = c(0, 0.5, 1, 0.5) + 0.1, omi = c(0, 0, 0.1, 0),
      mgp = c(0, 0.8, 0), las = 1)
  # identify variables to plot
  if ("enmo" %in% colnames(ts)) {
    acc = "enmo"
  } else if ("agcounts_vm" %in% colnames(ts)) {
    acc = "agcounts_vm"
  } else if ("LFEcounts_vm" %in% colnames(ts)) {
    acc = "LFEcounts_vm"
  } else {
    ts$fakeacc = 0
    acc = "fakeacc"
  }
  # find midnights
  nightsi = which(ts$timestamp$hour == 0 & ts$timestamp$min == 0 & ts$timestamp$sec == 0)
  # extend timestamp with one day before and one day after
  extendbf = extendafter = FALSE
  if (length(nightsi) == 0) {
    extendbf = extendafter = TRUE
  } else {
    if (nightsi[1] > 1) extendbf = TRUE
    if (max(nightsi) < nrow(ts)) extendafter = TRUE
  }
  if (extendbf == TRUE) {
    from = ts$timestamp[1] - 24*3600
    ln = length(seq(from, ts$timestamp[1] - epoch, by = epoch))
    time0 = deriveTimestamps(from = from, length = ln, epoch = epoch)
    ts0tmp = ts[1:nrow(time0), ]
    ignore = which(colnames(ts) %in% c("subject", "date", "time", "timestamp"))
    ts0tmp[, -ignore] = NA
    ts0tmp[, c("date", "time")] = time0
    ts0tmp$timestamp = seq(from, ts$timestamp[1] - epoch, by = epoch)
    ts = rbind(ts0tmp, ts)
  }
  if (extendafter == TRUE) {
    from = max(ts$timestamp) + epoch
    to = from + 24*3600
    ln = length(seq(from, to, by = epoch))
    time0 = deriveTimestamps(from = from, length = ln, epoch = epoch)
    ts0tmp = ts[1:ln,]
    ignore = which(colnames(ts) %in% c("subject", "date", "time", "timestamp"))
    ts0tmp[, -ignore] = NA
    ts0tmp[, c("date", "time")] = time0
    ts0tmp$timestamp = seq(from, to, by = epoch)
    ts = rbind(ts, ts0tmp)
  }
  # redefine midnights and trim data
  nightsi = grep("00:00:00", ts$time)
  ts = ts[nightsi[1]:max(nightsi),]
  nightsi = grep("00:00:00", ts$time)
  # options for plot
  ylim = c(0, quantile(ts[, acc], 0.99, na.rm = T)*1.3)
  ylim_tilt = c(min(ts$tilt, na.rm = T), max(ts$tilt, na.rm = T))
  for (i in 1:(length(nightsi) - 1)) {
    day = ts[nightsi[i]:nightsi[i + 1],]
    # x axis ticks
    look4times = paste(seq(0,22,by = 2), "00:00", sep = ":")
    look4times = ifelse(nchar(look4times) == 7, paste0("0", look4times), look4times)
    xticks = grep(paste(look4times, collapse = "|"), day$time)
    xnames = c(substr(look4times, 1, 2), "00")
    # plot
    par(mar = c(0, 0.5, 1, 0.5) + 0.1)
    plot(day[, "tilt"], type = "l", ylim = ylim_tilt,
         ylab = "", xlab = "", axes = F, bty = "n", col = "grey")
    title(main = as.character(day[1, "date"]), adj = 0)
    par(new = TRUE)
    plot(day[, acc], type = "l", ylim = ylim,
         ylab = "", xlab = "", axes = F, bty = "n")
    potential_sleepOnsetN1 = potential_sleepOnsetN2 = c()
    potential_sleepEndN1 = potential_sleepEndN2 = c()
    # mark classes
    par(mar = c(2, 0.5, 0, 0.5) + 0.1)
    plot(day[, acc], type = "n", ylim = c(0,2),
         ylab = "", xlab = "", axes = F, bty = "n")
    for (classi in 1:length(classes)) {
      st_en = find_start_end(ts = day, column = "activity", class = classes[classi])
      st = st_en$starts; en = st_en$ends
      if (length(st) > 0) {
        if (grepl("nighttime", classes[classi])) {
          st_after_6pm = which(day$time[st] > "18:00:00")
          nn = ifelse(length(st_after_6pm) > 0, 2, 1)
          st_bu = st; en_bu = en
          for (ni in 1:nn) {
            if (ni == 1) {
              if (length(st_after_6pm) > 0) {
                st = st_bu[-st_after_6pm]
                en = en_bu[-st_after_6pm]
              }
              if (length(st) > 0) {
                potential_sleepOnsetN1 = c(potential_sleepOnsetN1, min(st))
                potential_sleepEndN1 = c(potential_sleepEndN1, max(en))
              }
            } else {
              if (length(st_after_6pm) > 0) {
                st = st_bu[st_after_6pm]
                en = en_bu[st_after_6pm]
              }
              if (length(st) > 0) {
                potential_sleepOnsetN2 = c(potential_sleepOnsetN2, min(st))
                potential_sleepEndN2 = c(potential_sleepEndN2, max(en))
              }
            }
            if (length(st) > 0) {
              rect(xleft = st, xright = en, ybottom = 0, ytop = 1,
                   col = colors[classi], border = NA)
            }
          }
        } else {
          rect(xleft = st, xright = en, ybottom = 0, ytop = 1,
               col = colors[classi], border = NA)
        }
      }
    }
    # x axis
    axis(side = 1, at = xticks, labels = xnames, tick = FALSE, line = 0)
    # start_end night
    st = en = NULL
    if (length(potential_sleepOnsetN1) > 0) {
      st = c(st, min(potential_sleepOnsetN1))
      en = c(en, max(potential_sleepEndN1))
    }
    if (length(potential_sleepOnsetN2) > 0) {
      st = c(st, min(potential_sleepOnsetN2))
      en = c(en, max(potential_sleepEndN2))
    }
    if (length(st) > 0) {
      if (st[1] == 1) st = st[-1]
      if (max(en) > nrow(day)) en = en[-which.max(en)]
      abline(v = c(st, en), lwd = 2)
    }
  }
}
