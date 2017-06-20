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
#source('../NewR/makeNominalData.R')
library(neuroim)
library(ExPosition)

#######################################################
#### Convert trial design data to TR design data and save out csvs
## run.01.design<-read.csv(paste0(design.dir,'ds107_run-01_design.csv'))
## run.02.design<-read.csv(paste0(design.dir,'ds107_run-02_design.csv'))
# 
## run.01.TR.design<-durations.to.TR.design(onsets=run.01.design$seconds-1, durations = 1, condition = run.01.design$condition, 
#                                          TR.length = 3,save.csv=T,csv.fn=paste0(design.dir,'ds107_run-01_TR_DESIGN.csv'))
## run.02.TR.design<-durations.to.TR.design(onsets=run.02.design$seconds-1, durations = 1, condition = run.02.design$condition, 
#                                          TR.length = 3,save.csv=T,csv.fn=paste0(design.dir,'ds107_run-02_TR_DESIGN.csv'))
# 

### data should just be any .nii in the given directory.
sub.01 <- list(
  data = c(paste0(nii.dir,'sub-01/func/',data.pref,'sub-01',data.mid,'run-01',data.suff),  #run 1 4D time series data
           paste0(nii.dir,'sub-01/func/',data.pref,'sub-01',data.mid,'run-02',data.suff)), #run 2 4D time series data
  masks = mask,
  design = c(paste0(design.dir,'ds107_run-01_TR_DESIGN.csv'), #run 1 design conditions per TR
             paste0(design.dir,'ds107_run-02_TR_DESIGN.csv')) #run 2 design conditions per TR
)

sub.09 <- list(
  data = c(paste0(nii.dir,'sub-09/func/',data.pref,'sub-09',data.mid,'run-01',data.suff),
           paste0(nii.dir,'sub-09/func/',data.pref,'sub-09',data.mid,'run-02',data.suff)),
  masks = mask,
  design = c(paste0(design.dir,'ds107_run-01_TR_DESIGN.csv'),
             paste0(design.dir,'ds107_run-02_TR_DESIGN.csv'))
)

subj.list <- list(sub.01,sub.09)
names(subj.list) <- c("sub.01","sub.09")

### create data list structure for each participant
data.list <- subject.data.list(subj.list)

### do preprocessing for each subject individually (i.e., detrend)
data.list.preproc<-preproc.indiv(data.list)


### some stuff below will end up in concatenate.data.R
### and then a preproc function
## and then all embedded in a PCA function

## linear detrend, row normalize, then column center.
concat.data <- cbind(
  expo.scale(rowNorms(apply(data.list$S01$dataMatrix,2,function(x){resid( lm( x~seq(x) ) )}),type="z"),center=T,scale=F),
  expo.scale(rowNorms(apply(data.list$S02$dataMatrix,2,function(x){resid( lm( x~seq(x) ) )}),type="z"),center=T,scale=F),
  expo.scale(rowNorms(apply(data.list$S03$dataMatrix,2,function(x){resid( lm( x~seq(x) ) )}),type="z"),center=T,scale=F),
  expo.scale(rowNorms(apply(data.list$S04$dataMatrix,2,function(x){resid( lm( x~seq(x) ) )}),type="z"),center=T,scale=F)
)
participant.design <- c(
  rep("S01",ncol(data.list$S01$dataMatrix)),
  rep("S02",ncol(data.list$S02$dataMatrix)),
  rep("S03",ncol(data.list$S03$dataMatrix)),
  rep("S04",ncol(data.list$S04$dataMatrix))
)
## drop TRs

#dim(concat.data)

concat.data_dropped <- concat.data[-c(which(data.list$S01$dataDesign=="INSTRUCTION" | data.list$S01$dataDesign=="FIXATION")),]



## ROW DES
row.des <- makeNominalData(as.matrix(data.list$S01$dataDesign[-c(which(data.list$S01$dataDesign=="INSTRUCTION" | data.list$S01$dataDesign=="FIXATION")),1]))
col.des <- makeNominalData(as.matrix(participant.design))



pca <- gsvd(concat.data_dropped)
prettyPlot(pca$p,col=createColorVectorsByDesign(row.des)$oc,pch=20)
prettyPlot(pca$q,col=createColorVectorsByDesign(col.des)$oc,pch=20)

S01<-as.matrix(pca$q[which(col.des[,1]==1),1])
S02<-as.matrix(pca$q[which(col.des[,2]==1),1])
S03<-as.matrix(pca$q[which(col.des[,3]==1),1])
S04<-as.matrix(pca$q[which(col.des[,4]==1),1])

matrixToVolume(dataMatrix = S01, mask= data.list$S01$mask ,dataFileName = "S01.nii")
matrixToVolume(dataMatrix = S02, mask= data.list$S02$mask ,dataFileName = "S02.nii")
matrixToVolume(dataMatrix = S03, mask= data.list$S03$mask ,dataFileName = "S03.nii")
matrixToVolume(dataMatrix = S04, mask= data.list$S04$mask ,dataFileName = "S04.nii")






mcpls <- gsvd(t(row.des) %*% concat.data_dropped)
prettyPlot(mcpls$p,col=createColorVectorsByDesign(row.des)$gc,pch=20)
prettyPlot(mcpls$q,col=createColorVectorsByDesign(col.des)$oc,pch=20)

mcS01<-as.matrix(mcpls$q[which(col.des[,1]==1),1])
mcS02<-as.matrix(mcpls$q[which(col.des[,2]==1),1])
mcS03<-as.matrix(mcpls$q[which(col.des[,3]==1),1])
mcS04<-as.matrix(mcpls$q[which(col.des[,4]==1),1])

matrixToVolume(dataMatrix = mcS01, mask= data.list$S01$mask ,dataFileName = "mcS01.nii")
matrixToVolume(dataMatrix = mcS02, mask= data.list$S02$mask ,dataFileName = "mcS02.nii")
matrixToVolume(dataMatrix = mcS03, mask= data.list$S03$mask ,dataFileName = "mcS03.nii")
matrixToVolume(dataMatrix = mcS04, mask= data.list$S04$mask ,dataFileName = "mcS04.nii")



