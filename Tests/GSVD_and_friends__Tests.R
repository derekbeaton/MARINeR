rm(list=ls())
gc()
### SOME TESTS.

source('../NewR/GSVD_Functions.R')
source('../MARINeR/R/power.rebuild_matrix.R')
source('../MARINeR/R/invert.rebuild_matrix.R')
source('../MARINeR/R/tolerance.svd.R')


## I need to test the new versions of functions against the GSVD_Functions functions
## I also need to make sure I can easily get the results from CA (which means my GSVD is working completely).

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
X <- scale(hilbert(9)[, 1:6],scale=F)
XXt <- tcrossprod(X) #XXt / (X %*% t(X))
XtX <- crossprod(X) #XtX / (t(X) %*% X)



t.res <- tolerance.svd(XXt)
my.res <- my.svd(XXt)
svd.res <- svd(XXt)


XXt / power.rebuild_matrix(x = XXt,power=1)## technically a rebuild
#XXt - power.rebuild_matrix(x = XXt,power=1)## technically a rebuild

## sqrt tests.
sqrt.mat(XXt) / power.rebuild_matrix(x = XXt,power=.5)
sqrt.mat(XXt) / power.rebuild_matrix(x = XXt,power=1/2)



# svd.sqrt.mat(XXt) / sqrt.mat(XXt)
# svd.sqrt.mat(XXt) - sqrt.mat(XXt)
# svd.sqrt.mat(XXt) / power.rebuild_matrix(x = XXt,power=1/2)
# svd.sqrt.mat(XXt) - power.rebuild_matrix(x = XXt,power=1/2)




svd.pinv(XXt,tol = .Machine$double.eps) / invert.rebuild_matrix(x = XXt)
svd.pinv_sq(XXt,tol = .Machine$double.eps) / ginv(XXt)

library(MASS)
ginv(XXt) / svd.pinv(XXt,tol = .Machine$double.eps)
ginv(XXt) / invert.rebuild_matrix(x = XXt)


