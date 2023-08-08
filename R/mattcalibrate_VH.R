#' Title
#'
#' @param file
#' @param temp
#' @param sphere
#' @param Fs
#' @param chunksize
#' @param mintime
#'
#' @return
#' @export
#'
#' @examples
calibrate<-function(file,temp=FALSE,sphere=.3,Fs,chunksize=1,mintime=84)
{
  cat("Calibration in Progress\n")
  i=1 #defining variables that will be used later in calibration
  LD=2
  count=1
  NR = ceiling((90 * 10^6)/(Fs * 10)) + 1000
  if(temp==TRUE){
    hold<- matrix(9999,NR,7)
  }else{
  hold<- matrix(9999,NR,6) #defining matrix for identifying non movement periods
  }
  bsc_cnt = 0
  bsc_qc = data.frame(time = c(), size = c())
  file<-file[,-1]
  blocksize.start = blocksize = constant = round((14512 * (Fs/50)) * (chunksize * 0.5)) #variables used to read data in 12 hr chunks


  while (LD > 1) {

  if (i == 1) {
    cat(paste("\nLoading time-block: ", i, sep = ""))
  }
  else {
    cat(paste(" ", i, sep = ""))
  }

    acc = file[(1+(blocksize.start * 300 * (i -1))):(blocksize * 300) ,] #reading accel data in 12hr chunks
    blocksize = blocksize+constant
    LD = nrow(acc)
    use = (floor(LD/(3600 * Fs))) * (3600 * Fs)
    if (length(use) > 0) {
    if (use > 0) {
    data = as.matrix(acc[1:use, ])
    LD = nrow(data)
    dur = nrow(data)
    durexp = nrow(data)/(Fs * 3600)

  if(temp==TRUE){
    x = as.numeric(data[, 1])
    y = as.numeric(data[, 2])
    z = as.numeric(data[, 3])
    temp2 = as.numeric(data[,4])
  }else{
    x = as.numeric(data[, 1])
    y = as.numeric(data[, 2])
    z = as.numeric(data[, 3])
  }
  meanx<-slide(x,width = (Fs*10),FUN=mean)
  meany<-slide(y,width = (Fs*10),FUN=mean)
  meanz<-slide(z,width = (Fs*10),FUN=mean)
  sdx<-slide(x,width = (Fs*10),FUN=sd)
  sdy<-slide(y,width = (Fs*10),FUN=sd)
  sdz<-slide(z,width = (Fs*10),FUN=sd)
  if (temp == TRUE) {
    meantemp = slide(temp2,width = (Fs*10),FUN=mean)
  }

  hold[count:(count - 1 + length(meanx)),1]<-meanx #putting mean and sd into matrix as it gets read in 12hr chunks
  hold[count:(count - 1 + length(meanx)),2]<-meany
  hold[count:(count - 1 + length(meanx)),3]<-meanz
  hold[count:(count - 1 + length(meanx)),4]<-sdx
  hold[count:(count - 1 + length(meanx)),5]<-sdy
  hold[count:(count - 1 + length(meanx)),6]<-sdz
  if (temp ==TRUE){
    hold[count:(count - 1 + length(meanx)),7]<-meantemp
  }
  count = count + length(meanx)
  rm(x)
  rm(y)
  rm(z)
  gco = gc()
  memuse = gco[2, 2]
  bsc_qc = rbind(bsc_qc, c(memuse, Sys.time())) #lines 73-80 are to make adjustments in case calibration is using too much memory space in r
  if (memuse > 4000) {
    if (bsc_cnt < 5) {
      if ((chunksize * (0.8^bsc_cnt)) > 0.2) {
        constant = round(constant * 0.8)
        bsc_cnt = bsc_cnt + 1
      }
    }
  }
  }
  }
  i =i+1
  hold_temp = hold
  cut = which(hold_temp[, 1] == 9999)
  if (length(cut) > 0) {
    hold_temp = as.matrix(hold_temp[-cut, ]) #remove parts of matrix that do not have data
  }
  if((nrow(hold_temp) * 10)/3600>=mintime){ #stops reading in blocks of data if minimum time (84 hrs) is reached or end of data
    LD=0
  }
  if(nrow(hold_temp)>=(nrow(file)*1.5/(Fs*10))){
    LD=0
  }
  if((nrow(file)/(Fs*10))-nrow(hold_temp)<(6*60*12)){
    LD=0
  }
  }
hold<-na.omit(hold_temp)
nhoursused = (nrow(hold) * 10)/3600
  sdthresh = 0.013 #defines sd threshold for what is considered non-movement
  meanthresh = 2 #defines mean threshold for what is conisdered non-movement
  nomovement = which(hold[, 4] < sdthresh & hold[,5] < sdthresh & hold[, 6] < sdthresh &
                       abs(as.numeric(hold[,1])) < meanthresh & abs(as.numeric(hold[, 2])) <meanthresh & abs(as.numeric(hold[, 3])) <meanthresh)
  hold = as.matrix(hold[nomovement, ])
  nomovementhours = (nrow(hold) * 10)/3600
  cal.error.start = sqrt(as.numeric(hold[,1])^2 + as.numeric(hold[, 2])^2 + as.numeric(hold[,3])^2)
  cal.error.start = mean(abs(cal.error.start -1))
  aa<-center_radius(hold[,1:3])
  tel = 0
  for (axis in 1:3) {
    if (min(hold[, axis]) < -sphere &
        max(hold[, axis]) > sphere) {
      tel = tel + 1 #this is to check that there are enough data points around the "sphere"
    }
  }
  if (!tel == 4) {
    input<-hold[,1:3]
    if(temp == TRUE){inputtemp<-cbind(as.numeric(hold[,7]),
                                      as.numeric(hold[,7]),
                                      as.numeric(hold[,7]))} #establishes temperature vectors that will be applied to each axis during calibration
    else(inputtemp = matrix(0, nrow(input), ncol(input)))
    #cat("sphere populated")

  }else {

    #cat("not enough points around sphere to reliably perform autocalibration")
  }
  meantemp = mean(as.numeric(inputtemp[, 1]), na.rm = TRUE)
  inputtemp = inputtemp - meantemp
  ######################################## This is where autocalibration begins, looks like it uses gradient descent
  offset = rep(0, ncol(input))
  scale = rep(1, ncol(input))
  weights = rep(1, nrow(hold))
  tempoffset = rep(0, ncol(input))
  res = Inf
  maxiter = 1000
  tol = 1e-10
  for (iter in 1:maxiter) {
    curr = scale(input, center = -offset, scale = 1/scale) +
      scale(inputtemp, center = F, scale = 1/tempoffset) #changes data values after each iteration with new offsets and scales

    closestpoint = curr/sqrt(rowSums(curr^2))
    k = 1
    offsetch = rep(0, ncol(input)) #change in offset, will use linear model to obtain values, resets after each iteration
    scalech = rep(1, ncol(input)) #change in scale, will use linear model to obtain values, resets after each iteration
    toffch = rep(0, ncol(inputtemp)) #change in temp, will use linear model to obtain values, resets after each iteration
  #######
    if (length(which(is.na(closestpoint[,k, drop = F]) == TRUE)) > 0) {
      invi = which(is.na(closestpoint[, k, drop = F]) ==
                     TRUE)
      closestpoint = closestpoint[-invi, ]
      curr = curr[-invi, ]
      inputtemp = inputtemp[-invi, ]
      input = input[-invi, ]
      weights = weights[-invi]
    }
  ######
    for (k in 1:ncol(input)) {

      fobj = lm.wfit(cbind(1, curr[, k], inputtemp[,k]),
                     closestpoint[, k, drop = F], w = weights) #linear model that is applied to new data values after each iteration

      offsetch[k] = fobj$coef[1]
      scalech[k] = fobj$coef[2]
      if (temp == TRUE) {
        toffch[k] = fobj$coeff[3]
      }
      curr[, k] = fobj$fitted.values
    }
    offset = offset + offsetch/(scale * scalech) #sets new offset values after each iteration
    if (temp == TRUE) {
      tempoffset = tempoffset * scalech + toffch
    }
    scale = scale * scalech #sets new scale values after each iteration
    res = c(res, 3 * mean(weights * (curr - closestpoint)^2/sum(weights)))
    weights = pmin(1/sqrt(rowSums((curr - closestpoint)^2)),
                   1/0.01) #updates weight values
    if (abs(res[iter + 1] - res[iter]) < tol) #stops if change in decrease from previous iteration is below 1e-10
      break
  }
  if (temp == FALSE) {
    hold2 = scale(as.matrix(hold[, 1:3]),
                       center = -offset, scale = 1/scale)
  }
  else {
    yy = as.matrix(cbind(as.numeric(hold[, 7]),
                         as.numeric(hold[, 7]), as.numeric(hold[,
                                                                          7])))
    hold2 = scale(as.matrix(hold[, 1:3]),
                       center = -offset, scale = 1/scale) + scale(yy,
                                                                  center = rep(meantemp, 3), scale = 1/tempoffset)
  }

  cal.error.end = sqrt(hold2[, 1]^2 + hold2[,2]^2 + hold2[, 3]^2)
  cal.error.end = mean(abs(cal.error.end - 1))

  if(tel==3){
  cat(paste("\nHours Used: ",round(nomovementhours,1),"\n\n",sep = ""))
  cat("-----Before Calibration Results-----\n")

  cat(paste("VM Error (mg's): ",round(cal.error.start*1000,2),"\n",sep = ""))
  cat(paste("Center Error (x,y,z): ","(", aa$center[1],",",aa$center[2],",",
            aa$center[3],")\n",sep = ""))
  cat(paste("Radius Error: ",aa$radius,"\n",sep = ""))

  cat("\n-----After Calibration Results-----\n")

  cat(paste("VM Error (mg's): ",round(cal.error.end*1000,2),"\n",sep = ""))

  aa<-center_radius(hold2)
  cat(paste("Center Error (x,y,z): ","(", aa$center[1],",",aa$center[2],",",
            aa$center[3],")\n",sep = ""))
  cat(paste("Radius Error: ",aa$radius,"\n",sep = ""))
  }
  return(list(scale = scale, offset = offset,vm.error.start = cal.error.start, vm.error.end = cal.error.end,
              tel = tel))
}




