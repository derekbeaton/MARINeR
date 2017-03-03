#'
#'  @export
#'
#'  @title \code{invert.rebuild_matrix}: psuedo inverse and rebuild of a matrix
#'
#'  @description \code{invert.rebuild_matrix} takes in a matrix and will compute the psuedo-inverse via the singular value decomposition.
#'  Additionally, the psuedo-inverse can be computed for a lower rank estimate of the matrix.
#'
#'  @param x data matrix to compute the pseudo-inverse of
#'  @param k the number of components to retain in order to build a lower rank estimate of \code{x}
#'  @param ... parameters to pass through to \code{\link{tolerance.svd}}
#'
#'  @return
#'  The (possibly lower rank) psuedo-inverse of \code{x}
#'
#'  @seealso \code{\link{tolerance.svd}} and \code{\link{power.rebuild_matrix}}
#'
#'  @examples
#'  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
#'  X <- hilbert(9)[, 1:6]
#'  X.inv <- invert.rebuild_matrix(X)
#'  X.inv %*% X ## is approximately an identity.
#'
#'  @author Derek Beaton
#'
#'  @keywords multivariate, diagonalization, eigen, pseudo-inverse, Moore-Penrose
#'

invert.rebuild_matrix <- function(x, k=0, ...){
  ## actually, these should test if they are a vector and just return them as is.
  ## also test if diagonal, and if so, only return a vector.
  ## but maybe that's for later...


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
  return( (res$v[,comp.ret] * matrix(1/res$d[comp.ret],nrow(res$v[,comp.ret]),ncol(res$v[,comp.ret]),byrow=T)) %*% t(res$u[,comp.ret]) )
}
