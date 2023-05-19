library(splines)
## for bs function

## pass occur.cov and detect.cov as vectors
spline_wrapper <- function(occur.cov, detect.cov, num.knots = 7){
  m <- 1:num.knots
  psiBound <- c(floor(min(occur.cov)), ceiling(max(occur.cov)))
  pBound <- c(floor(min(detect.cov)), ceiling(max(detect.cov)))
  
  X.sp <- bs(occur.cov, knots = psiBound[1] + m * psiBound[2] / (numKnots + 1)) 
  X.p.sp <- bs(detect.cov, knots = pBound[1] + m * pBound[2] / (numKnots + 1)) 
  
  return(list(X.sp = X.sp, X.p.sp = X.p.sp))
}