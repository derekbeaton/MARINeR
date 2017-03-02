source('0_Utilities.R')
	##A whole brain and a VV mask are created in the 0_Utilities function.


##data
run1.data <- readNIfTI('../Data/filtered_func_data_fMRI1.nii',reorient=FALSE)


##Soon, we need to make a matrix to hold the data. 
##Here, the data are 4D:
	##X, Y, Z is 64 x 64 x 31
	##T (time) is 217, see:
print(run1.data)

##so we want 217 rows by (64 x 64 x 31) columns
	##that is, each time point/stimulus (217) x all voxels (64 x 64 x 31)
		##however, that includes a lot of non-brain voxels. So, which voxels do we want?


##Just the voxels for the brain.
brain.voxels <- which(wb.mask@.Data==1)
all.data.mat.r1 <- matrix(0,run1.data@dim[4],length(brain.voxels))
for(i in 1:run1.data@dim[4]){
	
	r1.tr.i <- run1.data@.Data[,,,i]
	all.data.mat.r1[i,] <- r1.tr.i[brain.voxels]
	
}


###TRs are 2s
##for both runs
	#first face at TR 12
	#last face at TR 209
##this is where things get weird. We know the HRF in response to stimulus doesn't start at 	the first stimulus presentation.
##The HRF ususally starts around 3-4s after presentation. So that means we want to take the 14th TR, and then the next 191 so we have 192 TRs that represent stimulus presentation.

##keep only relevant TRs:
data.mat.r1 <- all.data.mat.r1[14:(14+191),]


##pick a few random voxels and plot time courses:
plot.timecourse(data.mat.r1,columns=c(100,1000,5000,10000,50000))

##Now let's visualize the distribution of voxel intensities:
dev.new()
h<-hist(c(data.mat.r1),breaks=250,col="grey80",border=NA)



##visualize S.D. of voxels
r1.sd <- apply(data.mat.r1,2,sd)
plot.brain(r1.sd,wb.mask,brain.voxels)
dev.new()
h<-hist(r1.sd,breaks=500,col="grey80",border=NA)
abline(v=30,col="red",lwd=2)

##visualize the SNR of voxels
r1.snr <- apply(data.mat.r1,2,function(x){sqrt(mean(x)/sd(x))})
plot.brain(r1.snr,wb.mask,brain.voxels)
dev.new()
h<-hist(r1.snr,breaks=500,col="grey80",border=NA)
abline(v=7,col="red",lwd=2)


##Well, which voxels are those below those red line thresholds?
##SD
plot.brain(as.numeric(r1.sd < 30),wb.mask,brain.voxels)
##SNR
plot.brain(as.numeric(r1.snr < 7),wb.mask,brain.voxels)
##now together
plot.brain((as.numeric(r1.snr < 7) + as.numeric(r1.sd < 30)),wb.mask,brain.voxels)

###these look good to me to drop.
drop.vox <- which(r1.snr < 7 | r1.sd < 30)
brain.voxels.drop <- brain.voxels[drop.vox]
plot.brain(1,wb.mask, brain.voxels.drop)
### and these look good to me to keep.
keep.vox <- which(r1.snr >= 7 & r1.sd >= 30)
brain.voxels.notdropped <- brain.voxels[keep.vox]
plot.brain(1,wb.mask, brain.voxels.notdropped)

new.wb.mask <- wb.mask
new.wb.mask@.Data[brain.voxels.drop] <- 0
new.wb.mask@.Data[brain.voxels.notdropped] <- 1
image(new.wb.mask)


brain.voxels.keep <- which(new.wb.mask@.Data==1)

VV.voxels.keep <- which((new.wb.mask@.Data+VV.mask@.Data)==2)

