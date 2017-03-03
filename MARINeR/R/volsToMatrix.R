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
#'  @param dataVols A 4D nifti file of brain volumes across time 
#'  @param maskVol A 3D nifti file of a brain volume to be used as mask. All values >0 used inclusively
#'  
#'  @return 
#'  A list with 2 elements
#'  \item{dataMatrix} a matrix
#'  \item{mask} a \code{\link{DenseBrainVolume}} of the input mask. Contains \code{\link{BrainSpace}} information for \code{$dataMatrix}
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
    print('Mutli masking not supported yet :(')
  }else{
    mask.in<-loadVolume(maskVol)
  }
  
  vols.in<-loadVector(fileName = dataVols,mask=mask.in)
  dataMatrix<-vols.in@data

  return(list(dataMatrix=dataMatrix,mask=mask.in))
}