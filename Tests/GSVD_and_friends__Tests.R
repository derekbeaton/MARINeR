rm(list=ls())
gc()
### SOME TESTS.

library(ExPosition)

#source('../NewR/GSVD_Functions.R')
source('../MARINeR/R/power.rebuild_matrix.R')
source('../MARINeR/R/invert.rebuild_matrix.R')
#source('../MARINeR/R/invert.sqrt.rebuild_matrix.R')
source('../MARINeR/R/tolerance.svd.R')
source('../MARINeR/R/gsvd.R')
source('../MARINeR/R/isDiagonal.matrix.R')

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


library(MASS)
svd.pinv_sq(XXt,tol = .Machine$double.eps) / ginv(XXt)
ginv(XXt) / svd.pinv(XXt,tol = .Machine$double.eps)
ginv(XXt) / invert.rebuild_matrix(x = XXt)





#sqrt.mat(svd.pinv_sq(XXt)) / invert.sqrt.rebuild_matrix(XXt)
sqrt.mat(svd.pinv_sq(XXt,tol = .Machine$double.eps)) / power.rebuild_matrix(invert.rebuild_matrix(XXt),power = .5)


sqrt.mat(svd.pinv_sq(XXt,tol = .Machine$double.eps)) / power.rebuild_matrix(XXt,power = -1/2)
power.rebuild_matrix(invert.rebuild_matrix(XXt),power = .5) / power.rebuild_matrix(XXt,power = -1/2)



data(authors)
Observed <- authors$ca$data/sum(authors$ca$data)
rowW <- rowSums(Observed)
colW <- colSums(Observed)
Expected <- rowW %o% colW
Deviations <- Observed - Expected
ca.res <- gsvd(Deviations,diag(1/rowW),diag(1/colW))
ca.res2 <- gsvd(Deviations,1/rowW,1/colW)
fi <- diag(1/rowW) %*% ca.res$p %*% diag(ca.res$d)
fj <- diag(1/colW) %*% ca.res$q %*% diag(ca.res$d)



ep.res <- epCA(authors$ca$data,graphs=F)

ep.res$ExPosition.Data$fi / fi
ep.res$ExPosition.Data$fj / fj
ep.res$ExPosition.Data$pdq$Dv / ca.res$d

