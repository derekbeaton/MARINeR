
power.rebuild_matrix <- function(x, power = 1, k=0, ...){

  ## actually, these should test if they are a vector and just return them as is.
  ## also test if diagonal, and if so, only return a vector.
    ## but maybe that's for later...

  ##stolen from MASS::ginv()
  if (length(dim(x)) > 2L || !(is.numeric(x) || is.complex(x)))
    stop("power.rebuild_matrix: 'x' must be a numeric or complex matrix")
  if (!is.matrix(x))
    x <- as.matrix(x)

  if(k<=0){
    k <- min(nrow(x),ncol(x))
  }

  res <- tolerance.svd(x,...)  ## just go with the defaults of this or allow pass through?
  ## maybe pass through via '...' -- which would be my first time using that!

  comp.ret <- 1:min(length(res$d),k)
    ### this will cause a problem if k=1... but deal with it later.
  return( (res$u[,comp.ret] * matrix(res$d[comp.ret]^power,nrow(res$u[,comp.ret]),ncol(res$u[,comp.ret]),byrow=T)) %*% t(res$v[,comp.ret]) )

}
