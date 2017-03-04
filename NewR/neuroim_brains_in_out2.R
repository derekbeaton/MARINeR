#### workspace for testing out functions

rm(list=ls())
gc()

#######################################################
###### script for getting multiple peeps
source('./MARINeR/R/volsToMatrix.R')
source('./MARINeR/R/subject.data.list.R')
#source('../MARINeR/R/rowNorms.R')
#source('../MARINeR/R/expo.scale.R')
source('./MARINeR/R/gsvd.R')
source('./MARINeR/R/tolerance.svd.R')
source('./MARINeR/R/matrixToVolume.R')
#source('../NewR/makeNominalData.R')
library(neuroim)
library(ExPosition)

### data should just be any .nii in the given directory.
S01 <- list(
    data=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S01/filtered_func_data_fMRI1.nii',
           '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S01/filtered_func_data_fMRI2.nii'),
    masks=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S01/L_amygdala2example_func.nii',
            '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S01/R_amygdala2example_func.nii'),
    design=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/run1_des.csv',
             '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/run2_des.csv')
      )

S02 <- list(
  data=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S02/filtered_func_data_fMRI1.nii',
         '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S02/filtered_func_data_fMRI2.nii'),
  masks=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S02/L_amygdala2example_func.nii',
          '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S02/R_amygdala2example_func.nii'),
  design=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/run1_des.csv',
           '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/run2_des.csv')
)

S03 <- list(
  data=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S03/filtered_func_data_fMRI1.nii',
         '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S03/filtered_func_data_fMRI2.nii'),
  masks=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S03/L_amygdala2example_func.nii',
          '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S03/R_amygdala2example_func.nii'),
  design=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/run1_des.csv',
           '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/run2_des.csv')
)

S04 <- list(
  data=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S04/filtered_func_data_fMRI1.nii',
          '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S04/filtered_func_data_fMRI2.nii'),
  masks=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S04/L_amygdala2example_func.nii',
          '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S04/R_amygdala2example_func.nii'),
  design=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/run1_des.csv',
           '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/run2_des.csv')
)


subj.list <- list(S01,S02,S03,S04)
names(subj.list) <- c("S01","S02","S03","S04")

data.list <- subject.data.list(subj.list)


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



