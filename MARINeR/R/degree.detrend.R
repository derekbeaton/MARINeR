## a test is required:
  ## can I exp(log(x, base=deg) ~ seq()) and get the same as the current set up?
  ## many rows and degrees higher than 3 will fail at some point.
    ## THIS HSOULD BE GOOD NOW?

degree.detrend <- function(x,deg=1){
  deg.mat <- resid( lm( x ~ poly(1:nrow(x), degree = deg)) )
  rownames(deg.mat) <- rownames(x)
  colnames(deg.mat) <- colnames(x)
  return( deg.mat )
}				