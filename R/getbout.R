#' Title
#'
#' @param x
#' @param boutduration
#' @param boutcriter
#' @param closedbout
#' @param bout.metric
#' @param ws3
#'
#' @return
#' @export
#'
#' @examples
getbout<-function (x, boutduration, boutcriter = 0.8, closedbout = FALSE,
                   bout.metric = 1, ws3 = 15)
{
  p = which(x == 1)
  if (bout.metric == 1) {
    xt = x
    boutcount = rep(0, length(x))
    jmvpa = 1
    Lx = length(x)
    while (jmvpa <= length(p)) {
      endi = p[jmvpa] + boutduration
      run<-rle(x[p[jmvpa]:endi])
      if (endi <= Lx) {
        if (sum(x[p[jmvpa]:endi]) > (boutduration * boutcriter) & !any(run$lengths>=(60/ws3) & run$values==0)) {
          while (sum(x[p[jmvpa]:endi]) > ((endi - p[jmvpa]) *
                                          boutcriter) & endi <= Lx &
                                            !sum(x[(endi-((60/ws3)-1)):endi])==0 ){
            endi = endi + 1
          }
          select = p[jmvpa:max(which(p < endi))]
          jump = length(select)
          xt[select] = 2
          boutcount[p[jmvpa]:p[max(which(p < endi))]] = 1
        }
        else {
          jump = 1
          x[p[jmvpa]] = 0
        }
      }
      else {
        jump = 1
        if (length(p) > 1 & jmvpa > 2) {
          x[p[jmvpa]] = x[p[jmvpa - 1]]
        }
      }
      jmvpa = jmvpa + jump
    }
    x[which(xt == 2)] = 1
    if (length(which(xt == 1)) > 0)
      x[which(xt == 1)] = 0
    if (closedbout == TRUE) {
      x = boutcount
    }
  }

  x[which(xt != 2)] = 0
  x[which(xt == 2)] = 1


  invisible(list(x = x, boutcount = boutcount))
}
