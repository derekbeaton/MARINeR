
## RECTANGULAR MATRICES.
# THIS IS SQRT
# res <- my.svd(X,tol=tol)
# return( (res$u * matrix(sqrt(res$d),nrow(res$u),ncol(res$u),byrow=T)) %*% t(res$v) )

# THIS IS INVERSION VIA SVD.
# res <- my.svd(X,tol=tol)
# return( (res$v * matrix(1/res$d,nrow(res$v),ncol(res$v),byrow=T)) %*% t(res$u) )



### is it really this simple?
### no, not it's not.
invert.sqrt.rebuild_matrix <- function(x, k=0, ...){
  ### by default, this gives you back your matrix.
  ### SO DON'T BE A DUMMY.

  ##stolen from MASS::ginv()
  if (length(dim(x)) > 2L || !(is.numeric(x) || is.complex(x)))
    stop("invert.rebuild_matrix: 'x' must be a numeric or complex matrix")
  if (!is.matrix(x))
    x <- as.matrix(x)


  if(k<=0){
    k <- min(nrow(x),ncol(x))
  }
  res <- tolerance.svd(x,...)  ## just go with the defaults of this or allow pass through?
  ## maybe pass through via '...' -- which would be my first time using that!
  comp.ret <- 1:min(length(res$d),k)
  return( (res$v[,comp.ret] * matrix(sqrt(1/res$d[comp.ret]),nrow(res$v[,comp.ret]),ncol(res$v[,comp.ret]),byrow=T)) %*% t(res$u[,comp.ret]) )
}
