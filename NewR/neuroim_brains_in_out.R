#### brains in and out via neuroim

library(neuroim)
library(ExPosition)

dataVols<-'C:/Users/Jenny/Documents/projects/brainhack/degrafaces/nii/3780/MotCor/WmCsfV3780cond1+censor.nii'
maskVol<-'C:/Users/Jenny/Documents/projects/brainhack/degrafaces/masks/aal_numb_4mm_dgScrub.nii'

design<-read.csv('C:/Users/Jenny/Documents/projects/brainhack/degrafaces/design/rowDESIGN_run_face_vs_fix_TR144.csv')

volsToMatrix<-function(dataVols, maskVol){
  if(length(maskVol)==0){
    print('Please provide a mask for the data')
  }
  if(length(maskVol)>1){
    print('Mutli masking not supported yet :(')
  }else{
    mask.in<-loadVolume(maskVol)
  }
  
  vols.in<-loadVector(fileName = dataVols,mask=mask.in)
  dataMatrix<-vols.in@data
  #bspace<-BrainSpace(Dim=c(vols.in@space@Dim[1:3],1),spacing=vols.in@space@spacing, origin=vols.in@space@origin,axes=vols.in@space@axes,trans=vols.in@space@trans)
  bspace<-space(vols.in)
  
  return(list(bspace=bspace,mask=mask.in,dataMatrix=dataMatrix))
}

## get the brain matrix
data.out<-volsToMatrix(dataVols=dataVols,maskVol=maskVol)

## do some analysis
pca.res<-epPCA(DATA=data.out$dataMatrix,scale=T, center=T,graphs = F)
lv<-1
resMatrix<-as.matrix(pca.res$ExPosition.Data$fj[,lv])
data.fn<-'myBrain.nii'

## write out the results
matrixToVolume<-function(dataMatrix,dataFileName='myBrain.nii',bspace,mask){
  #change time series brain space to 1 volume brain space
  if(length(bspace@Dim)>3){
    bspace@Dim<-bspace@Dim[1:3]
  }
  vol.out<-SparseBrainVector(dataMatrix,space=bspace,mask=mask)
  writeVector(vol.out,data.fn)
}

matrixToVolume(dataMatrix = resMatrix, dataFileName = data.fn,bspace = data.out$bspace, mask=data.out$mask)
