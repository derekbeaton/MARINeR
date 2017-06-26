#### workspace for testing out functions

rm(list=ls())
gc()

#setwd('C:/Users/jrieck/Documents/projects/brainhack/BrainHack_TO_2017/')
#setwd('C:/Users/Jenny/Documents/projects/brainhack/BrainHack_TO_2017/')

#######################################################
###### specify directories
nii.dir<-'../data/ds107/nii/'
design.dir<-'../data/ds107/design/'

data.pref<-''
data.mid<-'_task-onebacktask_'
data.suff<-'_bold_MNI.nii.gz'
mask<-'../data/ds107/std_masks/aalnumb_4mm_occ_mtl.nii'


#######################################################
###### sourcing necessary scripts
source('../MARINeR/R/volsToMatrix.R')
source('../MARINeR/R/subject.data.list.R')
source('../MARINeR/R/gsvd.R')
source('../MARINeR/R/tolerance.svd.R')
source('../MARINeR/R/matrixToVolume.R')
source('../MARINeR/R/preproc.indiv.R')
source('../MARINeR/R/drop.TRs.R')

source('../MARINeR/R/gsr.R')
source('../MARINeR/R/degree.detrend.R')
source('../MARINeR/R/svd.low.rank.rebuild.R')
source('../MARINeR/R/svd.norm.R')

source('../MARINeR/R/concatenate.data.R')

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

## Drop TRs of non interest
data.list.dropped<-drop.TRs(data.list, c('drop'))

### do preprocessing for each subject individually (i.e., detrend)
data.list.preproc<-preproc.indiv(data.list.dropped)
  ## this above function handles everything.

## concatenate.
concat.data <- concatenate.data(data.list.preproc)

## PCA!
gres <- gsvd(concat.data)
  #gres1 <- gsvd((concat.data[,1:6233]+concat.data[,6234:ncol(concat.data)])/2)
prettyScree(gres$d^2)
prettyPlot(gres$fi,col=createColorVectorsByDesign(makeNominalData(data.list.preproc$sub.15$dataDesign))$oc)
prettyPlot(gres$fi,col=createColorVectorsByDesign(makeNominalData(data.list.preproc$sub.15$dataDesign))$oc,x_axis=2,y_axis=3)


