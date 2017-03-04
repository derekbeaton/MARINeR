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
#'  S01 <- list(
#'  data=c('../Data/S01/filtered_func_data_fMRI1.nii',
#'         '../Data/S01/filtered_func_data_fMRI2.nii'),
#'  masks=c('../Masks/S01/L_amygdala2example_func.nii',
#'          '../Masks/S01/R_amygdala2example_func.nii'),
#'  design=c('../Design/S01/run1_design.csv',
#'           '../Design/S01/run2_design.csv')
#'  )
#'  
#'  S02 <- list(
#'  data=c('../Data/S02/filtered_func_data_fMRI1.nii',
#'         '../Data/S02/filtered_func_data_fMRI2.nii'),
#'  masks=c('../Masks/S02/L_amygdala2example_func.nii',
#'          '../Masks/S02/R_amygdala2example_func.nii'),
#'  design=c('../Design/S02/run1_design.csv',
#'           '../Design/S02/run2_design.csv')
#'  )
#'  
#'  subj.list <- list(S01,S02)
#'  names(subj.list) <- c("S01","S02")
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
