#### workspace for testing out functions

#######################################################
###### script for getting multiple peeps
source('../MARINeR/R/volsToMatrix.R')
library(neuroim)

subjs.dir<-'/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data'
subjs<-dir(subjs.dir)
masks.dir<-'/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks'

mask1<-'L_amygdala2example_func.nii'
mask2<-'R_amygdala2example_func.nii'

run1<-'filtered_func_data_fMRI1.nii'
run2<-'filtered_func_data_fMRI2.nii'



### data should just be any .nii in the given directory.
S01 <- list(
    subj.data=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S01/filtered_func_data_fMRI1.nii',
           '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S01/filtered_func_data_fMRI2.nii'),
    masks=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S01/L_amygdala2example_func.nii',
            '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S01/R_amygdala2example_func.nii')
      )


peepDesign<-c()
peepsOut<-c()
for(s in 1:length(subjs)){ ## assumes everyone has the same number of scans
  maskVol<-c(paste(masks.dir,subjs[s],mask1,sep='/'),paste(masks.dir,subjs[s],mask2,sep='/'))
  dataVols<-c(paste(subjs.dir,subjs[s],run1,sep='/'),paste(subjs.dir,subjs[s],run2,sep='/'))

  peepMat<-volsToMatrix(dataVols, maskVol)
  peepsOut<-cbind(peepsOut,peepMat$dataMatrix)
  peepDesign<-cbind(peepDesign,matrix(s,1,dim(peepMat$dataMatrix)[2]))
}
runDesign<-peepMat$runDesign




