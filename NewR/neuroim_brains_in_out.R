#### workspace for testing out functions

#######################################################
###### script for getting multiple peeps
setwd("C:/Users/Jenny/Documents/projects/brainhack/BrainHack_TO_2017/MARINeR/")
source('./R/volsToMatrix.R')
library(neuroim)

subjs.dir<-'C:/Users/Jenny/Documents/projects/brainhack/nitp/Data/'
subjs<-dir(subjs.dir)
masks.dir<-('c:/Users/Jenny/Documents/projects/brainhack/nitp/Masks/')

mask1<-'L_amygdala2example_func.nii'
mask2<-'R_amygdala2example_func.nii'

run1<-'filtered_func_data_fMRI1.nii'
run2<-'filtered_func_data_fMRI2.nii'

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

#######################################################
#######################################################
#### brains in and out via neuroim
#### test space for the functions

library(neuroim)
library(ExPosition)

dataVols<-'C:/Users/Jenny/Documents/projects/brainhack/degrafaces/nii/3780/MotCor/WmCsfV3780cond1+censor.nii'
maskVol<-'C:/Users/Jenny/Documents/projects/brainhack/degrafaces/masks/aal_numb_4mm_dgScrub.nii'

design<-read.csv('C:/Users/Jenny/Documents/projects/brainhack/degrafaces/design/rowDESIGN_run_face_vs_fix_TR144.csv')

### Let's try loading in multple masks
data1<-'C:/Users/Jenny/Documents/projects/brainhack/nitp/S03/filtered_func_data_fMRI1.nii'
data2<-'C:/Users/Jenny/Documents/projects/brainhack/nitp/S03/filtered_func_data_fMRI2.nii'
mask1<-'C:/Users/Jenny/Documents/projects/brainhack/BrainHack_TO_2017/ExampleData/Masks/S03/L_amygdala2example_func.nii'
mask2<-'C:/Users/Jenny/Documents/projects/brainhack/BrainHack_TO_2017/ExampleData/Masks/S03/R_amygdala2example_func.nii'
maskVol<-c(mask1,mask2)
dataVols<-c(data1,data2)

volsToMatrix<-function(dataVols, maskVol){
  if(length(maskVol)==0){
    print('Please provide a mask for the data')
  }
  if(length(maskVol)>1){
    print('Multiple masks detected - they will be combined')
    masks.in<-loadVolumeList(maskVol)
    mspace<-space(masks.in)
    mspace@Dim<-mspace@Dim[1:3]
    mask.in<-DenseBrainVolume(apply(masks.in,c(1,2,3),sum),space=mspace)
  }else{
    mask.in<-loadVolume(maskVol)
  }
  
  if(length(dataVols)>1){
    print('Multiple runs detected - they will be concatinated')
  }
  dataMatrix<-c()
  runDesign<-c()
  for(r in 1:length(dataVols)){
    vols.in<-loadVector(fileName = dataVols[r],mask=mask.in)
    nvols<-vols.in@space@Dim[4]
    dataMatrix<-rbind(dataMatrix,vols.in@data)
    runDesign<-rbind(runDesign,matrix(r,nvols,1))
  }                
  
  #bspace<-BrainSpace(Dim=c(vols.in@space@Dim[1:3],1),spacing=vols.in@space@spacing, origin=vols.in@space@origin,axes=vols.in@space@axes,trans=vols.in@space@trans)
  #bspace<-space(vols.in)
  return(list(mask=mask.in,dataMatrix=dataMatrix,runDesign=runDesign))
  
}
#######################################################
## get the brain matrix
data.out<-volsToMatrix(dataVols=dataVols,maskVol=maskVol)

#######################################################
## do some analysis
pca.res<-epPCA(DATA=data.out$dataMatrix,scale=T, center=T,graphs = F)
lv<-1
resMatrix<-as.matrix(pca.res$ExPosition.Data$fj[,lv])
data.fn<-'myBrain_4.nii'

#######################################################
## write out the results
matrixToVolume<-function(dataMatrix,mask,dataFileName='myBrain.nii'){
  bspace<-space(mask)
  vol.out<-SparseBrainVector(dataMatrix,space=bspace,mask=mask)
  writeVector(vol.out,data.fn)
}

matrixToVolume(dataMatrix = resMatrix, mask=data.out$mask,dataFileName = data.fn)

#######################################################
#### brain visualization
#rm(list=ls())
library(oro.nifti)
nii.in<-'myBrain.nii'
brain<-readNIfTI(nii.in)
image(brain)







