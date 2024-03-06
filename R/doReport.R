#' Generates Reports
#'
#' @description
#' Generates day-level and person-level reports#'
#'
#' @param files2summarize List of files to be summarized in reports.
#'
#' @return No returns, it only saves csv files in results folder.
#' @export
doReport = function(files2summarize) {
  # daysummary ---------------
  DS = daysummary = NULL
  for (i in 1:length(files2summarize)) {
    load(files2summarize[i])
    if (is.null(DS)) DS = daysummary else DS = plyr::rbind.fill(DS, daysummary)
  }
  fn = gsub("summary", "results", files2summarize[1])
  fn = file.path(dirname(fn), "daysummary.csv")
  write.csv(DS, file = fn, row.names = FALSE, na = "")
  # personsummary ---------------
  takeFirst = function(x) x[1]
  PS = NULL
  PS = tryCatch(aggregate(DS, by = list(DS$ID), FUN = mean, na.rm = TRUE),
                warning = function(e) aggregate(DS, by = list(DS$ID), FUN = takeFirst))
  d = grep("timestamp_|is_weekend", colnames(PS))
  PS = PS[, -d]
  colnames(PS) = gsub("date", "start_date", colnames(PS))
  colnames(PS) = gsub("weekday", "start_weekday", colnames(PS))
  fn = gsub("daysummary", "recordsummary", fn)
  write.csv(PS, file = fn, row.names = FALSE, na = "")
}
