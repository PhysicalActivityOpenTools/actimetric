#' Derives timestamps by epoch
#'
#'
#' @param from Numeric with starting time for timestamps in UTC format.
#' @param length Numeric with the length of the desired timestamp.
#' @param epoch Numeric with epoch length in seconds.
#'
#' @return Timestamp in "%Y-%m-%d %H:%M:%OS" format
#' @export
#'
deriveTimestamps = function(from, length, epoch) {
  s.t2 = from + epoch*(0:(length - 1))
  class(s.t2) = c('POSIXt','POSIXct')
  date = format(s.t2, "%Y-%m-%d")
  time = format(s.t2, "%H:%M:%OS")
  # NAs = which(is.na(time))
  # if (length(NAs) > 0) time[NAs] = "00:00:00.000"
  # # s.date = format(s.time2, "%Y-%m-%d")
  # # s.t2 = format(s.time2, "%H:%M:%OS")
  return(cbind(date, time))
}
