#' Calibrates Raw Accelerometer Data
#' @description
#' Calibrates the raw accelerometer signal based on the algorithm published
#' in
#'
#' @param raw Matrix containing accelerometer signal and, if available, temperature data.
#' @param sf Sampling frequency in Hertz.
#' @param verbose Logical (default = TRUE) indicating whether progress messages should
#' be printed in the console
#'
#' @return List with calibration coefficients and calibrated raw data
#' @export
#' @author Matthew N. Ahmadi <matthew.ahmadi@sydney.edu.au>
#'
calibrateRaw = function(raw, sf, verbose = TRUE) {
  # Original code provided by Matthew N. Ahmadi
  calCoefs = NULL; vm.error.st = NULL; vm.error.end = NULL
  # define internal functions (only used here) -----
  centerRadius <- function(aa) {
    A <- matrix(1,nrow(aa), 4)
    A[ ,1:3] <- aa[, 1:3]
    f <- rowSums(aa^2)
    C <- lm.fit(A, f)$coefficients
    t <- (C[1]*C[1]) + (C[2]*C[2]) + (C[3]*C[3]) + C[4]
    r <- round(sqrt(t),3)
    C <- round(C,3)
    return(list(center = C[1:3], radius = r))
  }
  GN = function(V, verbose = TRUE) {
    # ------
    f0 = function(Vx,Vy,Vz,Mxx,Mxy,Mxz,Myy,Myz,Mzz,Bx,By,Bz) {
      (Mxx*(Bx - Vx) + Mxy*(By - Vy) + Mxz*(Bz - Vz))^2 + (Mxy*(Bx - Vx) + Myy*(By - Vy) + Myz*(Bz - Vz))^2 + (Mxz*(Bx - Vx) + Myz*(By - Vy) + Mzz*(Bz - Vz))^2 - 1
    }
    f1 = function(Vx,Vy,Vz,Mxx,Mxy,Mxz,Myy,Bx,By,Bz) {
      2*(Bx - Vx)*(Mxx*(Bx - Vx) + Mxy*(By - Vy) + Mxz*(Bz - Vz))
    }
    f2 = function(Vx,Vy,Vz,Mxx,Mxy,Mxz,Myy,Myz,Bx,By,Bz){
      2*(By - Vy)*(Mxx*(Bx - Vx) + Mxy*(By - Vy) + Mxz*(Bz - Vz)) + 2*(Bx - Vx)*(Mxy*(Bx - Vx) + Myy*(By - Vy) + Myz*(Bz - Vz))
    }
    f3 = function(Vx,Vy,Vz,Mxx,Mxy,Mxz,Myz,Mzz,Bx,By,Bz) {
      2*(Bx - Vx)*(Mxz*(Bx - Vx) + Myz*(By - Vy) + Mzz*(Bz - Vz)) + 2*(Bz - Vz)*(Mxx*(Bx - Vx) + Mxy*(By - Vy) + Mxz*(Bz - Vz))
    }
    f4 = function(Vx,Vy,Vz,Mxy,Myy,Myz,Bx,By,Bz) {
      2*(By - Vy)*(Mxy*(Bx - Vx) + Myy*(By - Vy) + Myz*(Bz - Vz))
    }
    f5 = function(Vx,Vy,Vz,Mxy,Mxz,Myy,Myz,Mzz,Bx,By,Bz) {
      2*(By - Vy)*(Mxz*(Bx - Vx) + Myz*(By - Vy) + Mzz*(Bz - Vz)) + 2*(Bz - Vz)*(Mxy*(Bx - Vx) + Myy*(By - Vy) + Myz*(Bz - Vz))
    }
    f6 = function(Vx,Vy,Vz,Mxz,Myz,Mzz,Bx,By,Bz) {
      2*(Bz - Vz)*(Mxz*(Bx - Vx) + Myz*(By - Vy) + Mzz*(Bz - Vz))
    }
    f7 = function(Vx,Vy,Vz,Mxx,Mxy,Mxz,Myy,Myz,Mzz,Bx,By,Bz) {
      2*Mxx*(Mxx*(Bx - Vx) + Mxy*(By - Vy) + Mxz*(Bz - Vz)) + 2*Mxy*(Mxy*(Bx - Vx) + Myy*(By - Vy) + Myz*(Bz - Vz)) + 2*Mxz*(Mxz*(Bx - Vx) + Myz*(By - Vy) + Mzz*(Bz - Vz))
    }
    f8 = function(Vx,Vy,Vz,Mxx,Mxy,Mxz,Myy,Myz,Mzz,Bx,By,Bz) {
      2*Mxy*(Mxx*(Bx - Vx) + Mxy*(By - Vy) + Mxz*(Bz - Vz)) + 2*Myy*(Mxy*(Bx - Vx) + Myy*(By - Vy) + Myz*(Bz - Vz)) + 2*Myz*(Mxz*(Bx - Vx) + Myz*(By - Vy) + Mzz*(Bz - Vz))
    }
    f9 = function(Vx,Vy,Vz,Mxx,Mxy,Mxz,Myy,Myz,Mzz,Bx,By,Bz) {
      2*Mxz*(Mxx*(Bx - Vx) + Mxy*(By - Vy) + Mxz*(Bz - Vz)) + 2*Myz*(Mxy*(Bx - Vx) + Myy*(By - Vy) + Myz*(Bz - Vz)) + 2*Mzz*(Mxz*(Bx - Vx) + Myz*(By - Vy) + Mzz*(Bz - Vz))
    }
    # -------------
    lambda = 1; kl = 0.01; tol = 1e-9; Rold = 100000; itr = 1000;
    Mxx0 = 5; Mxy0 = 0.5; Mxz0 = 0.5; Myy0 = 5; Myz0 = 0.5; Mzz0 = 5;
    Bx0 = 0.5; By0 = 0.5; Bz0 = 0.5;
    v = c(Mxx0, Mxy0, Mxz0, Myy0, Myz0, Mzz0, Bx0, By0, Bz0)
    m = nrow(V);
    R = rep(0, m); J = matrix(0,m,9);
    Vx = V[,1]; Vy = V[,2]; Vz = V[,3];
    vold = NULL
    # ---------------
    for (n in 1:itr) {
      #Calculate the Jacobian at every iteration
      for (i in 1:length(Vx)) {
        R[i]    =  f0(Vx[i], Vy[i], Vz[i], Mxx0,Mxy0,Mxz0,Myy0,Myz0,Mzz0,Bx0,By0,Bz0)
        J[i, 1] =  f1(Vx[i],Vy[i],Vz[i],Mxx0,Mxy0,Mxz0,Myy0,Bx0,By0,Bz0)
        J[i, 2] =  f2(Vx[i],Vy[i],Vz[i],Mxx0,Mxy0,Mxz0,Myy0,Myz0,Bx0,By0,Bz0)
        J[i, 3] =  f3(Vx[i],Vy[i],Vz[i],Mxx0,Mxy0,Mxz0,Myz0,Mzz0,Bx0,By0,Bz0)
        J[i, 4] =  f4(Vx[i],Vy[i],Vz[i],Mxy0,Myy0,Myz0,Bx0,By0,Bz0)
        J[i, 5] =  f5(Vx[i],Vy[i],Vz[i],Mxy0,Mxz0,Myy0,Myz0,Mzz0,Bx0,By0,Bz0)
        J[i, 6] =  f6(Vx[i],Vy[i],Vz[i],Mxz0,Myz0,Mzz0,Bx0,By0,Bz0)
        J[i, 7] =  f7(Vx[i], Vy[i], Vz[i], Mxx0,Mxy0,Mxz0,Myy0,Myz0,Mzz0,Bx0,By0,Bz0)
        J[i, 8] =  f8(Vx[i], Vy[i], Vz[i], Mxx0,Mxy0,Mxz0,Myy0,Myz0,Mzz0,Bx0,By0,Bz0)
        J[i, 9] =  f9(Vx[i], Vy[i], Vz[i], Mxx0,Mxy0,Mxz0,Myy0,Myz0,Mzz0,Bx0,By0,Bz0)
      }
      Rnew = sqrt(sum(R^2))
      H = solve(t(J) %*% J)
      D = t(J) %*% R
      v = v - lambda*t(H %*% D)
      if (Rnew <= Rold) lambda = lambda - kl*lambda else lambda = kl*lambda
      if (n > 1) {
        abs(max(2*(v - vold)/(v + vold)))
        if (abs(max(2*(v - vold)/(v + vold))) <= tol) {
          if (verbose) cat('\nConvergence achieved in the calibration process\n');
          break
        }
      }
      Mxx0 = v[1]; Mxy0 = v[2]; Mxz0 = v[3]; Myy0 = v[4]; Myz0 = v[5]; Mzz0 = v[6];
      Bx0 = v[7]; By0 = v[8]; Bz0 = v[9]; vold = v; Rold = Rnew
    }
    return(list(offset = v[c(7,8,9)],scale = v[c(1,4,6)]))
  }
  # main script ------------------------------------
  if (verbose) cat("\n\nCalibration in Progress\n")
  # use first 72 hours for calibration
  raw_bu = raw # back up of raw
  vm = sqrt(rowSums(raw[,1:3]^2))
  # 30-second means/sds per axis
  Gx = slide(raw[,1], 30*sf, FUN = mean)
  Gy = slide(raw[,2], 30*sf, FUN = mean)
  Gz = slide(raw[,3], 30*sf, FUN = mean)
  G = cbind(Gx, Gy, Gz)
  vm.sd = slide(vm, 30*sf, FUN = sd)
  vm = slide(vm, 30*sf, FUN = mean)
  # select sphere points
  e = which(vm.sd < .013 & vm < 2 & vm > 0)
  if (length(e) > 0) {
    G = G[e,]
    vm = vm[e]
    # hours used for calibration
    hrs = round(nrow(G)/2/60, 2)
    # calibration error start
    vm.error.st = round(mean(abs(vm - 1))*1000, 2)
    # Center and radius of the sphere before calibration
    aa = centerRadius(G)
    # get calibration coeffients (offset and scale)
    calCoefs = GN(G, verbose = verbose)
    # rescale data and calculate error after calibration
    G[,1] = calCoefs$scale[1]*(G[,1] - calCoefs$offset[1])
    G[,2] = calCoefs$scale[2]*(G[,2] - calCoefs$offset[2])
    G[,3] = calCoefs$scale[3]*(G[,3] - calCoefs$offset[3])
    vm = sqrt(rowSums(G^2))
    vm.error.end = round(mean(abs(vm - 1))*1000,2)
    aa2 = centerRadius(G)
    # console messages
    if (verbose) {
      cat(paste0("Hours Used: ", hrs, "\n\n"))
      cat("-----Before Calibration Results-----\n")
      cat(paste("VM Error (mg's): ",vm.error.st,"\n",sep = ""))
      cat(paste("Center Error (x,y,z): ","(", aa$center[1],",",aa$center[2],",",
                aa$center[3],")\n",sep = ""))
      cat(paste("Radius Error: ",aa$radius,"\n",sep = ""))
      cat("\n-----After Calibration Results-----\n")
      cat(paste("VM Error (mg's): ",vm.error.end,"\n",sep = ""))
      cat(paste("Center Error (x,y,z): ","(", aa2$center[1],",",aa2$center[2],",",
                aa2$center[3],")\n",sep = ""))
      cat(paste("Radius Error: ",aa2$radius,"\n",sep = ""))
    }
    return(list(calCoefs = calCoefs, vm.error.st = vm.error.st, vm.error.end = vm.error.end))
  } else {
    warning("Device not calibrated because it did not record enough orientation changes")
  }
  # return
  return(list(calCoefs = calCoefs, vm.error.st = vm.error.st, vm.error.end = vm.error.end))
}
