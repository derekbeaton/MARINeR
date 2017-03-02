
## RECTANGULAR MATRICES.
# THIS IS SQRT
# res <- my.svd(X,tol=tol)
# return( (res$u * matrix(sqrt(res$d),nrow(res$u),ncol(res$u),byrow=T)) %*% t(res$v) )

# THIS IS INVERSION VIA SVD.
# res <- my.svd(X,tol=tol)
# return( (res$v * matrix(1/res$d,nrow(res$v),ncol(res$v),byrow=T)) %*% t(res$u) )



  ### is it really this simple?
power.invert_matrix <- function(x, power = 1, invert = F, ...){
  ### by default, this gives you back your matrix.
  ### SO DON'T BE A DUMMY.

  ##stolen from MASS::ginv()
  if (length(dim(x)) > 2L || !(is.numeric(x) || is.complex(x)))
    stop("power.invert_matrix: 'x' must be a numeric or complex matrix")
  if (!is.matrix(x))
    x <- as.matrix(x)

  res <- tolerance.svd(x,...)  ## just go with the defaults of this or allow pass through?
                                  ## maybe pass through via '...' -- which would be my first time using that!

  if(invert){
    return( (res$v * matrix(1/(res$d^power),nrow(res$v),ncol(res$v),byrow=T)) %*% t(res$u) )
  }else{
    return( (res$u * matrix(res$d^power,nrow(res$u),ncol(res$u),byrow=T)) %*% t(res$v) )
  }

}
