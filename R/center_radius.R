#' Title
#'
#' @param aa
#'
#' @return
#' @export
#'
#' @examples
center_radius <- function(aa){

x <- aa[,1]
y <- aa[,2]
z <- aa[,3]

A <- matrix(1,length(x),4)
A[,1:3] <- aa[,1:3]

f <- rep(0,length(x))
f <- (x*x) + (y*y) + (z*z)

#solve(A,f)


#qr.solve(A,f)
C <- lm.fit(A, f)$coefficients

t <- (C[1]*C[1]) + (C[2]*C[2]) + (C[3]*C[3]) + C[4]
r <- round(sqrt(t),3)
C <- round(C,3)
return(list(center = C[1:3], radius = r))
}
