source('../MARINeR/R/gsr.R')
source('../MARINeR/R/degree.detrend.R')

##some tests
test.mat <-do.call(rbind,lapply(1:25,function(x){rnorm(n = 100, mean = x, sd = 1)}))

gsr.test_mean <- gsr(test.mat)
gsr.test_median <- gsr(test.mat,gsr.vals="median")
gsr.test_prespec <- gsr(test.mat,gsr.vals=1:25)


deg.det_1 <- degree.detrend(test.mat,1)
deg.det_2 <- degree.detrend(test.mat,2)
deg.det_3 <- degree.detrend(test.mat,3)

