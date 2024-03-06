#' Get Blocksize for Reading File in Chunks
#'
#' @param file Character with file path to raw acceleration file
#' @param sf Numeric with sampling frequency in Hertz
#' @param monc Code for monitor as extracted from \code{GGIR::g.inspectfile}
#' @param dformat Code for file format as extracted from \code{GGIR::g.inspectfile}
#'
#' @return Block size for reading chunk of data
#' @export
getBlocksizeReadaccfile = function(file, sf, monc, dformat) {
  blocksize = round(14512 * (sf/50))
  if (monc == GGIR:::MONITOR$GENEA) blocksize = round(21467 * (sf/80))
  if (monc == GGIR:::MONITOR$ACTIGRAPH && dformat == GGIR:::FORMAT$CSV) blocksize = round(blocksize)#round(blocksize/5)
  if (monc == GGIR:::MONITOR$ACTIGRAPH && dformat == GGIR:::FORMAT$GT3X) blocksize = (24 * 3600)
  if (monc == GGIR:::MONITOR$AXIVITY && dformat == GGIR:::FORMAT$CWA) {
    if (utils::packageVersion("GGIRread") >= "0.3.1") {
      # 24-hour block.
      # CWA data blocks can have 40, 80 or 120 samples each; we'll take 80 as the average number.
      blocksize = round(24 * 3600 * sf / 80)
    } else {
      blocksize = round(blocksize * 1.0043)
    }
  }
  if (monc == GGIR:::MONITOR$AXIVITY && dformat == GGIR:::FORMAT$CSV) blocksize = round(blocksize)
  if (monc == GGIR:::MONITOR$MOVISENS) blocksize = sf * 60 * 1440
  if (monc == GGIR:::MONITOR$VERISENSE && dformat == GGIR:::FORMAT$CSV) blocksize = round(blocksize)
  return(blocksize)
}
