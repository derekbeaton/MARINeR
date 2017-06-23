source('../MARINeR/R/tolerance.svd.R')
source('../MARINeR/R/gsvd.R')
source('../MARINeR/R/gsr.R')
source('../MARINeR/R/degree.detrend.R')
source('../MARINeR/R/svd.low.rank.rebuild.R')
source('../MARINeR/R/svd.norm.R')


##some tests
test.mat <-do.call(rbind,lapply(1:25,function(x){rnorm(n = 100, mean = x, sd = 1)}))

gsr.test_mean <- gsr(test.mat)
gsr.test_median <- gsr(test.mat,gsr.vals="median")
gsr.test_prespec <- gsr(test.mat,gsr.vals=1:25)


deg.det_1 <- degree.detrend(test.mat,1)
deg.det_2 <- degree.detrend(test.mat,2)
deg.det_3 <- degree.detrend(test.mat,3)



  ## must build very specific matrices for this.
library(ExPosition)
data(beer.tasting.notes)
dat.X <- expo.scale(beer.tasting.notes$data)
## now SVD low rank rebuild...
full.rank.rebuild <- svd.low.rank.rebuild(dat.X)
rank.2.rebuild <- svd.low.rank.rebuild(dat.X,rank.rebuild = 2)
rank.3.rebuild <- svd.low.rank.rebuild(dat.X,rank.rebuild = 3)
rank.75perc.rebuild <- svd.low.rank.rebuild(dat.X,rank.rebuild = .75)
stupid.rank.rebuild <- svd.low.rank.rebuild(dat.X,rank.rebuild = -5)
  ## full rank or default option?
  ## a single component might be an issue?
rank.1.rebuild <- svd.low.rank.rebuild(dat.X,rank.rebuild = 1)
  ## a weird one...
rank.135.rebuild <- svd.low.rank.rebuild(dat.X,rank.rebuild = c(1,3,5))


  ## specialized rebuild before the norming step.
res <- epPCA(dat.X,F,F,graphs=F)
dat.X1 <- res$ExPosition.Data$pdq$p[,1:3] %*% diag(c(3,2,1)) %*% t(res$ExPosition.Data$pdq$q[,1:3])

## now SVD norms
svd.norm.res <- svd.norm(dat.X1)



