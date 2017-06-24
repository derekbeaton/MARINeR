#  Brain Volumes (4D) to a Matrix: A function that utilizeing neurim functions
#  to transform a 4D brain volume array to a time/TR by voxels matrix
#'
#'  @export
#'
#'  @title \code{matrixToVolume}: transforms a 1D matrix to a brain volume
#'
#'  @description \code{matrixToVolume} transforms a 1D matrix of voxel values \code{dataMatrix} to a brain volume
#'  using specifications in a masking file \code{maskvol} (used to generate the brian matrix)
#'
#'  @param dataMatrix a 1D matrix (n voxels x 1) of values to put into the brain volume
#'  @param mask a \code{\link{DenseBrainVolume}} mask containing \code{\link{BrainSpace}} information of the voxels
#'  @param dataFileName a string output name for the brain volume (with extension .nii)

#'
#'  @return
#'  a .nii brain volume with filename \code{dataFileName}
#'
#'  @seealso \code{\link{writeVector}}, \code{\link{SparseBrainVector}}
#'
#'  @author Jenny Rieck

matrixToVolume<-function(dataMatrix,mask,dataFileName='myBrain.nii'){
  bspace<-space(mask)
  vol.out<-SparseBrainVector(dataMatrix,space=bspace,mask=mask)
  writeVector(vol.out,dataFileName)
  #return(vol.out)
}
