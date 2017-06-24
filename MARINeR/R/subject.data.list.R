#  wrapper function for volsToMatrix for  multiple subjects
#'
#'  @export
#'  @title \code{volsToMatrix}: transforms a 4D brain volumes to a matrix
#'
#'  @description \code{subject.data.list} is a wrapper function for volstoMatrix
#'  
#'  @param subj.list is a list of subjects in which each subject is a list with three variables (\code{$data}, \code{$masks}, \code{$design}) 
#'  indicating source file names for input to \code{\link{volsToMatrix}}
#'  
#'  @return 
#'  A  variable with several levels for each participant with output of \code{\link{volsToMatrix}}
#'  
#'  @seealso \code{\link{volsToMatrix}}
#'  
#'  @example 
#'  
#'  sub.09 <- list(
#'  data=c('./data/ds107/nii/sub-09_task-onebacktask_run-01_bold_MNI.nii.gz',
#'         './data/ds107/nii/sub-09_task-onebacktask_run-02_bold_MNI.nii.gz'),
#'  masks=c('./data/ds107/std_masks/aalnumb_4mm_occ_mtl.nii',
#'          './data/ds107/std_masks/aalnumb_4mm_occ_mtl.nii'),
#'  design=c('./data/d107/design/ds107_run-01_165TR_DESIGN.csv',
#'           './data/d107/design/ds107_run-02_166TR_DESIGN.csv')
#'  )
#'  
#'  sub.15 <- list(
#'  data=c('./data/ds107/nii/sub-15_task-onebacktask_run-01_bold_MNI.nii.gz',
#'         './data/ds107/nii/sub-15_task-onebacktask_run-02_bold_MNI.nii.gz'),
#'  masks=c('./data/ds107/std_masks/aalnumb_4mm_occ_mtl.nii',
#'          './data/ds107/std_masks/aalnumb_4mm_occ_mtl.nii'),
#'  design=c('./data/d107/design/ds107_run-01_165TR_DESIGN.csv',
#'           './data/d107/design/ds107_run-02_166TR_DESIGN.csv')
#'  )
#'  
#'  subj.list <- list(sub.09,sub.15)
#'  names(subj.list) <- c("sub.09","sub.15")
#'  data.list <- subject.data.list(subj.list)


subject.data.list <- function(subj.list){

  masked.subject.data <- vector("list",length(subj.list))
  names(masked.subject.data) <- names(subj.list)

  for(s in 1:length(subj.list)){
    print(paste('running subject#', s, 'of', length(subj.list)))
    masked.subject.data[[s]] <- volsToMatrix(subj.list[[s]]$data, subj.list[[s]]$masks,subj.list[[s]]$design)
  }

  return(masked.subject.data)

}
