#  Brain Volumes (4D) to a Matrix: A function that utilizeing neurim functions 
#  to transform a 4D brain volume array to a time/TR by voxels matrix
#'
#'  @export
#'
#'  @title \code{volsToMatrix}: transforms a 4D brain volumes to a matrix
#'
#'  @description \code{volsToMatrix} transforms a brain volumes array (4D nifti) \code{dataVols} to a time 
#'  by voxels matrix \code{$dataMatrix} using an inclusion brain mask \code{maskVol}. Utilizes functions in 
#'  the package neuroim
#'
#'  @param dataVols A 4D nifti file of brain volumes across time. If multiple 4D nii files are input, they will be concatenated along the rows
#'  @param maskVol A 3D nifti file of a brain volume to be used as mask. All values >0 used inclusively. For more than 2 masks, provide filenames in a list and the masks will be sumed
#'  
#'  @return 
#'  A list with 3 elements
#'  \item{dataMatrix} a matrix
#'  \item{mask} a \code{\link{DenseBrainVolume}} of the input mask. Contains \code{\link{BrainSpace}} information for \code{$dataMatrix}
#'  \item{maskDesign} a matrix specifying which column belongs to which input run
#'  
#'  @seealso \code{\link{loadVolume}}, \code{\link{loadVector}}, \code{\link{DenseBrainVolume}}
#'  
#'  @examples
#'  dataVols<-'my4Dnifti.nii'
#'  maskVol<-'my3Dmasknifti.nii'
#'  brainMatrix<-volsToMatrix(dataVols,maskVol)
#'  
#'  @author Jenny Rieck


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
    print('Multiple runs detected - they will be concatenated')
  }
  dataMatrix<-c()
  runDesign<-c()
  for(r in 1:length(dataVols)){
    vols.in<-loadVector(fileName = dataVols[r],mask=mask.in)
    nvols<-vols.in@space@Dim[4]
    dataMatrix<-rbind(dataMatrix,vols.in@data)
    runDesign<-rbind(runDesign,matrix(r,nvols,1))
  } 
  return(list(mask=mask.in,dataMatrix=dataMatrix,runDesign=runDesign))
}