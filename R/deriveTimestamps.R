#' Derives timestamps by epoch
#'
#'
#' @param from Numeric with starting time for timestamps in UTC format.
#' @param length Numeric with the length of the desired timestamp.
#' @param epoch Numeric with epoch length in seconds.
#' @param tz A character string specifying the time zone to be used for the conversion.
#'   Examples include `"UTC"`, `"America/New_York"`, or `"Europe/Berlin"`.
#'   If not specified, the system's default time zone is used. Time zone handling affects
#'   how character or numeric inputs are interpreted and displayed.
#'   A full list of time zone identifiers can be found on
#'   [Wikipedia](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones).
#'
#' @return Timestamp in "%Y-%m-%d %H:%M:%OS" format
#' @export
#' @author Jairo H. Migueles <jairo@jhmigueles.com>
#' @author Matthew N. Ahmadi <matthew.ahmadi@sydney.edu.au>
#'
deriveTimestamps = function(from, length, epoch, tz = "") {
  s.t2_numeric = from + epoch*(0:(length - 1))
  s.t2 = as.POSIXct(s.t2_numeric, origin = "1970-1-1", tz = tz)
  date = format(s.t2, "%Y-%m-%d", tz = tz)
  time = format(s.t2, "%H:%M:%OS", tz = tz)
  return(cbind(date, time))
}
