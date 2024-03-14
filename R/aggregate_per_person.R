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
  # rows 2 keep

  # aggregate dataset
  PS = NULL
  PS = tryCatch(aggregate(daysummary, by = list(daysummary$ID), FUN = mean, na.rm = TRUE),
                warning = function(e) aggregate(daysummary, by = list(daysummary$ID), FUN = takeFirst))
  d = grep("timestamp_|is_weekend", colnames(PS))
  PS = PS[, -d]
  colnames(PS) = gsub("date", "start_date", colnames(PS))
  colnames(PS) = gsub("weekday", "start_weekday", colnames(PS))
  return(PS)
}
