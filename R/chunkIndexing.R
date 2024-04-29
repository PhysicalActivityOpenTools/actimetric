#' Derives Indices to Read Data in Chunks
#'
#' @description
#' Function used to derive indices to read raw data in 24-hour chunks.
#'
#' @param prevChunk Integer (default = 0) with the chunk index read in the
#' previous iteration.
#' @param sf Numeric (default = NULL) indicating the sampling frequency in Hertz.
#' @param rawEnd Numeric (default = NULL) indicating \code{nrow(raw)}.
#'
#' @return Indices to be read in current iteration
#' @export
#' @author Matthew N. Ahmadi <matthew.ahmadi@sydney.edu.au>
chunkIndexing = function(prevChunk = 0, sf = NULL, rawEnd = NULL) {
  # Original code provided by Matthew N. Ahmadi
  #variables used to read data in 24 hr increment
  constant = (sf*60*60*24)
  lastChunk = FALSE
  # define chunk to be read
  chunk = prevChunk + 1
  increment = constant * chunk
  # if remaining data is less than 24 hrs, set to end of data
  if (increment >= rawEnd) {
    increment = rawEnd;  lastChunk = TRUE
  }
  # indices to be read
  select = (1 + (constant * (chunk - 1))):(increment)
  return(list(select = select, lastChunk = lastChunk))
}
