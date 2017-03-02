


##Drop 'em!
data.mat.r1.wb <- data.mat.r1[, which(brain.voxels %in% brain.voxels.keep)]
data.mat.r1.vv <- data.mat.r1[, which(brain.voxels %in% VV.voxels.keep)]


##Re-visualize:
##Now let's visualize the distribution of voxel intensities:
dev.new()
h<-hist(c(data.mat.r1.wb),breaks=250,col="grey80",border=NA)
dev.new()
h<-hist(c(data.mat.r1.vv),breaks=250,col="grey80",border=NA)
##visualize S.D. of voxels
r1.sd.wb <- apply(data.mat.r1.wb,2,sd)
r1.sd.vv <- apply(data.mat.r1.vv,2,sd)
dev.new()
h<-hist(r1.sd.wb,breaks=500,col="grey80",border=NA)
abline(v=30,col="red",lwd=2)
dev.new()
h<-hist(r1.sd.vv,breaks=500,col="grey80",border=NA)
abline(v=30,col="red",lwd=2)

##visualize the SNR of voxels
r1.snr.wb <- apply(data.mat.r1.wb,2,function(x){sqrt(mean(x)/sd(x))})
r1.snr.vv <- apply(data.mat.r1.vv,2,function(x){sqrt(mean(x)/sd(x))})
dev.new()
h<-hist(r1.snr.wb,breaks=500,col="grey80",border=NA)
abline(v=7,col="red",lwd=2)
dev.new()
h<-hist(r1.snr.vv,breaks=500,col="grey80",border=NA)
abline(v=7,col="red",lwd=2)
	##Looks kind of OK!


##linear detrend across voxels
data.mat.r1.wb.ld <- apply(data.mat.r1.wb,2,lin.det)
data.mat.r1.vv.ld <- apply(data.mat.r1.vv,2,lin.det)
#data.mat.r1.orig <- apply(data.mat.r1,2,lin.det)

##pick a few random voxels and plot time courses (these are the same as above):
plot.timecourse(data.mat.r1.wb.ld,columns=c(100,1000,5000,10000,40000))
##pick a few random voxels and plot time courses (these are the same as above):
plot.timecourse(data.mat.r1.vv.ld,columns=c(100,100,500,1000,2000))
##pick a few random voxels and plot time courses (these are the same as above):
#plot.timecourse(data.mat.r1.orig,columns=c(100,1000,5000,10000,50000))

###Recall the rows are time points (TRs), and the columns are voxels.
##We have removed time and drift effects from the voxels (down the columns).
##We now need to normalize the brain at each time point so it is comparable.
	##We will center and normalize, a.k.a., z-score 

##z-score the rows
data.mat.r1.wb.norm <- rowNorms(data.mat.r1.wb.ld,'z')
data.mat.r1.vv.norm <- rowNorms(data.mat.r1.vv.ld,'z')
#data.mat.r1.orig.norm <- rowNorms(data.mat.r1.orig,'z')


# save(data.mat.r1.vv.norm,file='../Cleaned/data.mat.r1.vv.norm.rda')
# vv.voxels <- which(brain.voxels %in% VV.voxels.keep)
# save(vv.voxels,file='../Cleaned/vv.voxels.rda')
# save(VV.voxels.keep,file='../Cleaned/VV.voxels.keep.rda')
# save(VV.mask,file='../Cleaned/VV.mask.rda')



# save(data.mat.r1.a.norm,file='../Cleaned/data.mat.r1.a.norm.rda')
# save(data.mat.r1.orig.norm,file='../Cleaned/data.mat.r1.orig.norm.rda')
# save(data.mat.r1.a,file='../Cleaned/data.mat.r1.a.rda')
# save(data.mat.r1.orig,file='../Cleaned/data.mat.r1.orig.rda')

# save(brain.voxels,file='../Cleaned/brain.voxels.rda')
# save(brain.voxels.keep,file='../Cleaned/brain.voxels.keep.rda')

