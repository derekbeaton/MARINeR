#### brains in and out via neuroim

library(neuroim)
library(ExPosition)

data.dir<-'C:/Users/Jenny/Documents/projects/brainhack/degrafaces/nii/3780/MotCor/'
data.fn<-'WmCsfV3780cond1+censor.nii'
mask.dir<-'C:/Users/Jenny/Documents/projects/brainhack/degrafaces/masks/'
mask.fn<-'aal_numb_4mm_dgScrub.nii'
design<-read.csv('C:/Users/Jenny/Documents/projects/brainhack/degrafaces/design/rowDESIGN_run_face_vs_fix_TR144.csv')

mask.in<-loadVolume(paste(mask.dir,mask.fn,sep='/'))

##for a 4D time series
#run.in<-loadVector(paste(data.dir,data.fn,sep='/')) 
##for a time x voxel matrix
run.in.masked<-loadVector(paste(data.dir,data.fn,sep='/'),mask=mask.in)

### do shit to the time series x voxel matrix
### here I'm just doing a stupid mean centering
#data.proc<-run.in.masked@data - mean(run.in.masked@data)

### actualy we don't want to output a time series, we want to output a single volume
## we will do a pca with Expo
pca.res<-epPCA(DATA=run.in.masked@data,scale=T, center=T,DESIGN = design,make_design_nominal = F,graphs = F)
#rownames(pca.res$ExPosition.Data$fi)<-c(1:144)
#windows()
#prettyPlot(pca.res$ExPosition.Data$fi,col = pca.res$Plotting.Data$fi.col,display_names=T)
lv<-1
res.out<-as.matrix(pca.res$ExPosition.Data$fj[,lv])

### create the brain space data needed to write out the nii (just grab info from the data read in)
bspace.dat<-run.in.masked
bspace<-BrainSpace(Dim=c(bspace.dat@space@Dim[1:3],1),spacing=bspace.dat@space@spacing, origin=bspace.dat@space@origin,axes=bspace.dat@space@axes,trans=bspace.dat@space@trans)

## Write out the processed data matrix
vol.out<-SparseBrainVector(res.out,space=bspace,mask=mask.in)

out.fn<-'cond1_fj_lv1.nii'
writeVector(vol.out,out.fn)

