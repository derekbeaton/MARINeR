#### brains in and out via neuroim

library(neuroim)

data.dir<-'C:/Users/Jenny/Documents/projects/brainhack/degrafaces/nii/3780/MotCor/'
data.fn<-'WmCsfV3780cond1+censor.nii'
mask.dir<-'C:/Users/Jenny/Documents/projects/brainhack/degrafaces/masks/'
mask.fn<-'aal_numb_4mm_dgScrub.nii'
out.fn<-'myProcessedData.nii'

mask.in<-loadVolume(paste(mask.dir,mask.fn,sep='/'))

##for a 4D time series
#run.in<-loadVector(paste(data.dir,data.fn,sep='/')) 
##for a time x voxel matrix
run.in.masked<-loadVector(paste(data.dir,data.fn,sep='/'),mask=mask.in)

### create the brain space data needed to write out the nii (just grab info from the data read in)
bspace.dat<-run.in.masked
bspace<-BrainSpace(Dim=bspace.dat@space@Dim,spacing=bspace.dat@space@spacing, origin=bspace.dat@space@origin,axes=bspace.dat@space@axes,trans=bspace.dat@space@trans)

### do shit to the time series x voxel matrix
### here I'm just doing a stupid mean centering
data.proc<-run.in.masked@data - mean(run.in.masked@data)

## Write out the processed data matrix
data.out<-SparseBrainVector(data.proc,space=bspace,mask=mask.in)
writeVector(data.out,out.fn)

