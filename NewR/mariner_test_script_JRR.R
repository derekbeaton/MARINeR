#### workspace for testing out functions

rm(list=ls())
gc()

#setwd('C:/Users/jrieck/Documents/projects/brainhack/BrainHack_TO_2017/')
#setwd('C:/Users/Jenny/Documents/projects/brainhack/BrainHack_TO_2017/')

#######################################################
###### specify directories
nii.dir<-'./data/ds107/nii/'
design.dir<-'./data/ds107/design/'

data.pref<-''
data.mid<-'_task-onebacktask_'
data.suff<-'_bold_MNI.nii.gz'
mask<-'./data/ds107/std_masks/aalnumb_4mm_occ_mtl.nii'


#######################################################
###### sourcing necessary scripts
source('./MARINeR/R/volsToMatrix.R')
source('./MARINeR/R/subject.data.list.R')
#source('../MARINeR/R/rowNorms.R')
#source('../MARINeR/R/expo.scale.R')
source('./MARINeR/R/gsvd.R')
source('./MARINeR/R/tolerance.svd.R')
source('./MARINeR/R/matrixToVolume.R')
source('./MARINeR/R/preproc.indiv.R')
source('./TempCode/PreProc_Funcs.R')
#source('../NewR/makeNominalData.R')
library(neuroim)
library(ExPosition)

#######################################################
#### Convert trial design data to TR design data and save out csvs

### data should just be any .nii in the given directory.
sub.09 <- list(
  data = c(paste0(nii.dir,'sub-09/func/',data.pref,'sub-09',data.mid,'run-01',data.suff),  #run 1 4D time series data
           paste0(nii.dir,'sub-09/func/',data.pref,'sub-09',data.mid,'run-02',data.suff)), #run 2 4D time series data
  masks = mask,
  design = c(paste0(design.dir,'ds107_run-01_165TR_DESIGN.csv'), #run 1 design conditions per TR
             paste0(design.dir,'ds107_run-02_166TR_DESIGN.csv')) #run 2 design conditions per TR
)

sub.15 <- list(
  data = c(paste0(nii.dir,'sub-15/func/',data.pref,'sub-15',data.mid,'run-01',data.suff),
           paste0(nii.dir,'sub-15/func/',data.pref,'sub-15',data.mid,'run-02',data.suff)),
  masks = mask,
  design = c(paste0(design.dir,'ds107_run-01_165TR_DESIGN.csv'),
             paste0(design.dir,'ds107_run-02_166TR_DESIGN.csv'))
)

subj.list <- list(sub.09,sub.15)
names(subj.list) <- c("sub.09","sub.15")

### create data list structure for each participant
data.list <- subject.data.list(subj.list)

### do preprocessing for each subject individually (i.e., detrend)
data.list<-preproc.indiv(data.list)


### some stuff below will end up in concatenate.data.R
### and then a preproc function
## and then all embedded in a PCA function

## linear detrend, row normalize, then column center.
concat.data <- cbind(
  expo.scale(rowNorms(apply(data.list$sub.09$dataMatrixPreproc,2,function(x){resid( lm( x~seq(x) ) )}),type="z"),center=T,scale=F),
  expo.scale(rowNorms(apply(data.list$sub.15$dataMatrixPreproc,2,function(x){resid( lm( x~seq(x) ) )}),type="z"),center=T,scale=F)
  )
participant.design <- c(
  rep("sub.09",ncol(data.list$sub.09$dataMatrixPreproc)),
  rep("sub.15",ncol(data.list$sub.15$dataMatrixPreproc))
)
## drop TRs

#dim(concat.data)

## input data with nuisance volumes already dropped - should happen much much earlier
concat.data_dropped <- concat.data[-c(which(data.list$sub.09$dataDesign=="drop" | data.list$sub.09$dataDesign=="FIXATION")),]



## ROW DES
row.des <- makeNominalData(as.matrix(data.list$sub.09$dataDesign[-c(which(data.list$sub.09$dataDesign=="drop" | data.list$sub.09$dataDesign=="FIXATION")),1]))
col.des <- makeNominalData(as.matrix(participant.design))



pca <- gsvd(concat.data_dropped)
prettyPlot(pca$p,col=createColorVectorsByDesign(row.des)$oc,pch=20)
prettyPlot(pca$q,col=createColorVectorsByDesign(col.des)$oc,pch=20)

sub.09<-as.matrix(pca$q[which(col.des[,1]==1),1])
sub.15<-as.matrix(pca$q[which(col.des[,2]==1),1])


matrixToVolume(dataMatrix = sub.09, mask= data.list$sub.09$mask ,dataFileName = "sub_09_pca.nii")
matrixToVolume(dataMatrix = sub.15, mask= data.list$sub.09$mask ,dataFileName = "sub_15_pca.nii")



#############


mcpls <- gsvd(t(row.des) %*% concat.data_dropped)
prettyPlot(mcpls$p,col=createColorVectorsByDesign(row.des)$gc,pch=20)
prettyPlot(mcpls$q,col=createColorVectorsByDesign(col.des)$oc,pch=20)

mcsub.09<-as.matrix(mcpls$q[which(col.des[,1]==1),1])
mcsub.15<-as.matrix(mcpls$q[which(col.des[,2]==1),1])


matrixToVolume(dataMatrix = mcsub.09, mask= data.list$sub.09$mask ,dataFileName = "mcsub_09_pca.nii")
matrixToVolume(dataMatrix = mcsub.15, mask= data.list$sub.09$mask ,dataFileName = "mcsub_15_pca.nii")



