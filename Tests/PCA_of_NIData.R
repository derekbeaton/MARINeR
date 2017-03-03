rm(list=ls())
gc()
### SOME TESTS.

library(ExPosition)

source('../MARINeR/R/power.rebuild_matrix.R')
source('../MARINeR/R/invert.rebuild_matrix.R')
source('../MARINeR/R/tolerance.svd.R')
source('../MARINeR/R/gsvd.R')
source('../MARINeR/R/isDiagonal.matrix.R')

load('../OldR/ForMARINeR/DESIGN.rda')
load('../OldR/ForMARINeR/data.mat.r1.vv.norm.rda')



##epPCA test

ep.res <- epPCA(data.mat.r1.vv.norm,F,F,graphs=F)


pca.res1 <- gsvd(DAT = data.mat.r1.vv.norm)
  ## too slow...
pca.res <- gsvd(DAT = data.mat.r1.vv.norm, diag(1,nrow(data.mat.r1.vv.norm)), diag(1,ncol(data.mat.r1.vv.norm)))


pca.res$d / pca.res1$d
pca.res$p / pca.res1$p
pca.res$q / pca.res1$q

pca.res$d / ep.res$ExPosition.Data$pdq$Dv
pca.res$p / ep.res$ExPosition.Data$pdq$p
pca.res$q / ep.res$ExPosition.Data$pdq$q



prettyPlot(pca.res$p %*% diag(pca.res$d),col=createColorVectorsByDesign(DES)$oc)
prettyPlot(pca.res1$p %*% diag(pca.res1$d),col=createColorVectorsByDesign(DES)$oc)


LW <- invert.rebuild_matrix(tcrossprod(as.matrix(DES)))
RW <- invert.rebuild_matrix(crossprod(data.mat.r1.vv.norm))
cca.res <- gsvd(DAT = data.mat.r1.vv.norm, LW=LW,RW=RW)

prettyPlot(LW %*% cca.res$p %*% diag(cca.res$d), col=createColorVectorsByDesign(DES)$oc)
