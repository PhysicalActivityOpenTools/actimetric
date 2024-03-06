#' Read excel files
#'
#' @param file Character with path to file
#' @param tibble Read tidyverse tibbles
#'
#' @return List with a data frame per excel sheet.
#' @export
read_excel_allsheets <- function(file, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(file)
  x <- lapply(sheets, function(X) readxl::read_excel(file, sheet = X))
  if (!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
