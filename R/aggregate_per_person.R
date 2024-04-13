#' Aggregate Per Recording
#'
#' @description
#' Generates person/recording-level reports
#'
#' @param daysummary Data frame generated in \link{aggregate_per_date}.
#' @param n_valid_hours Numeric (default = 0) with absolute number of valid hours required to consider a day valid
#' @param n_valid_hours_awake Numeric (default = 0) with absolute number of awake valid hours required to consider a day valid
#' @param n_valid_hours_nighttime Numeric (default = 0) with absolute number of nighttime valid hours required to consider a day valid
#'
#' @return Data frame with aggregates of time spent in classes per recording.
#' @export
aggregate_per_person = function(daysummary, n_valid_hours = 0,
                                n_valid_hours_awake = 0, n_valid_hours_nighttime = 0) {
  # internal function
  takeFirst = function(x) x[1]
  # valid rows
  dur_valid_day = (daysummary$dur_total_fullday_min - daysummary$dur_total_nonwear_min) / 60
  valid = which(dur_valid_day >= n_valid_hours)
  if ("dur_total_awake_min" %in% colnames(daysummary)) {
    dur_valid_awake = (daysummary$dur_total_awake_min - daysummary$dur_total_nonwear_min) / 60
    dur_valid_nighttime = daysummary$dur_total_nighttime_min / 60
    valid = which(dur_valid_day >= n_valid_hours &
                    dur_valid_awake >= n_valid_hours_awake &
                    dur_valid_nighttime >= n_valid_hours_nighttime)
  }
  # subset daysummary
  PS = NULL
  if (length(valid) > 0) {
    total_days = aggregate(!is.na(date) ~ ID, data = daysummary, FUN = sum)
    total_wd = aggregate(is_weekend == 0 ~ ID, data = daysummary, FUN = sum)
    total_we = aggregate(is_weekend == 1 ~ ID, data = daysummary, FUN = sum)
    # keep only valid days
    daysummary = daysummary[valid,]
    valid_days = aggregate(!is.na(date) ~ ID, data = daysummary, FUN = sum)
    valid_wd = aggregate(is_weekend == 0 ~ ID, data = daysummary, FUN = sum)
    valid_we = aggregate(is_weekend == 1 ~ ID, data = daysummary, FUN = sum)
    # aggregate dataset
    PS = tryCatch(aggregate(daysummary, by = list(daysummary$ID), FUN = mean, na.rm = TRUE),
                  warning = function(e) aggregate(daysummary, by = list(daysummary$ID), FUN = takeFirst))
    d = grep("timestamp_|is_weekend", colnames(PS))
    PS = PS[, -d]
    colnames(PS) = gsub("^date", "start_date", colnames(PS))
    colnames(PS) = gsub("^weekday", "start_weekday", colnames(PS))
    # weekday average
    if (any(daysummary$is_weekend == 0)) {
      weekdays = daysummary[which(daysummary$is_weekend == 0),]
      WD = tryCatch(aggregate(weekdays, by = list(weekdays$ID), FUN = mean, na.rm = TRUE),
                    warning = function(e) aggregate(weekdays, by = list(weekdays$ID), FUN = takeFirst))
      d = grep("timestamp_|is_weekend", colnames(WD))
      WD = WD[, -d]
      colnames(WD) = gsub("^date", "start_date", colnames(WD))
      colnames(WD) = gsub("^weekday", "start_weekday", colnames(WD))
    }
    # weekend average
    if (any(daysummary$is_weekend == 1)) {
      weekends = daysummary[which(daysummary$is_weekend == 1),]
      WE = tryCatch(aggregate(weekends, by = list(weekends$ID), FUN = mean, na.rm = TRUE),
                    warning = function(e) aggregate(weekends, by = list(weekends$ID), FUN = takeFirst))
      d = grep("Group|timestamp_|is_weekend", colnames(WE))
      WE = WE[, -d]
      colnames(WE) = gsub("^date", "start_date", colnames(WE))
      colnames(WE) = gsub("^weekday", "start_weekday", colnames(WE))
    }
    # merge
    if (exists("WD") & exists("WE")) {
      WDWE = merge(WD, WE, by = "ID", suffixes = c("_WD", "_WE"), all = TRUE)
      PS = merge(PS, WDWE, by = "ID", all = TRUE)
      rm(WE, WD)
    }
    # add total and valid days
    total_wdwe = merge(total_wd, total_we, by = "ID", all = TRUE)
    total_days = merge(total_days, total_wdwe, by = "ID")
    colnames(total_days) = c("ID", "nRecordedDays", "nRecordedWD", "nRecordedWE")
    PS = merge(PS, total_days, by = "ID")
    valid_wdwe = merge(valid_wd, valid_we, by = "ID", all = TRUE)
    valid_days = merge(valid_days, valid_wdwe, by = "ID")
    colnames(valid_days) = c("ID", "nValidDays", "nValidWD", "nValidWE")
    PS = merge(PS, valid_days, by = "ID")
    # organize
    firstCols = c("ID", "classifier", "start_date", "start_weekday",
                  "nRecordedDays", "nRecordedWD", "nRecordedWE",
                  "nValidDays", "nValidWD", "nValidWE")
    rest = colnames(PS)[which(!colnames(PS) %in% firstCols)]
    PS = PS[, c(firstCols, rest)]
  }
  return(PS)
}
