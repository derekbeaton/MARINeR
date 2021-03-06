% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsvd.R
\name{gsvd}
\alias{gsvd}
\title{@export}
\usage{
gsvd(DAT, LW = NaN, RW = NaN, nu = min(dim(DAT)), nv = min(dim(DAT)),
  k = 0, tol = .Machine$double.eps)
}
\description{
@title \code{gsvd}: the generalized singular value decomposition.
}
\details{
@description \code{gsvd} takes in left (\code{LW}) and right (\code{RW}) constraints (usually diagonal matrices, but any positive semi-definite matrix is fine) that are applied to the data (\code{DAT})
  Left and right constraints are used for the orthogonality conditions.

@param DAT a data matrix to decompose
 @param LW \bold{L}eft \bold{W}eights -- the constraints applied to the left side (rows) of the matrix and thus left singular vectors
 @param RW \bold{R}ight \bold{W}eights -- the constraints applied to the right side (rows) of the matrix and thus right singular vectors
 @param nu the number of left singular vectors to be computed. Default is \code{min(dim(x))}
 @param nv the number of right singular vectors to be computed. Default is \code{min(dim(x))}
 @param k total number of components to return though the full variance (based on nu and nv) will still be returned (see \code{Dv.orig})
 @param tol A tolerance level for eliminating (tiny variance or negative or imaginary) components. Default is .Machine$double.eps

@return
 A list with seven elements:
 \item{p} a vector containing the singular values of x of length min(n, p) but also accounting for \code{tol}.
 \item{q} a vector containing the singular values of x of length min(n, p) but also accounting for \code{tol}.
 \item{u} a matrix whose columns contain the left singular vectors of x, present if nu > 0. Dimension c(n, nu) but also accounting for \code{tol}.
 \item{v} a matrix whose columns contain the left singular vectors of x, present if nv > 0. Dimension c(p, nv) but also accounting for \code{tol}.
 \item{d} a vector containing the singular values of x of length min(n, p) but also accounting for \code{tol}.
 \item{d.orig} a vector containing the singular values of x of length min(n, p) but also accounting for \code{tol}.
 \item{tau} a vector that contains the (original) explained variance per component.

@seealso \code{\link{tolerance.svd}} and \code{\link{svd}}

@examples
 ## an example with correspondence analysis.
 authors <- rbind(
   cbind(7836, 13112, 6026),
   cbind(53655, 102383, 42413),
   cbind(115615, 184541, 59226),
   cbind(161926, 340479, 62754),
   cbind(38177, 105101, 12670),
   cbind(46371, 58367, 14299)
   )

Observed <- authors/sum(authors)
 rowW <- rowSums(Observed)
 colW <- colSums(Observed)
 Expected <- rowW %o% colW
 Deviations <- Observed - Expected
 ca.res <- gsvd(Deviations,diag(1/rowW),diag(1/colW))
 fi <- diag(1/rowW) %*% ca.res$p %*% diag(ca.res$d)
 fj <- diag(1/colW) %*% ca.res$q %*% diag(ca.res$d)

@author Derek Beaton
 @keywords multivariate, diagonalization, eigen
}
