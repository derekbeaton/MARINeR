#### workspace for testing out functions

#######################################################
###### script for getting multiple peeps
source('../MARINeR/R/volsToMatrix.R')
source('../MARINeR/R/subject.data.list.R')
library(neuroim)

### data should just be any .nii in the given directory.
S01 <- list(
    data=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S01/filtered_func_data_fMRI1.nii',
           '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S01/filtered_func_data_fMRI2.nii'),
    masks=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S01/L_amygdala2example_func.nii',
            '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S01/R_amygdala2example_func.nii')
      )

S02 <- list(
  data=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S02/filtered_func_data_fMRI1.nii',
         '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S02/filtered_func_data_fMRI2.nii'),
  masks=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S02/L_amygdala2example_func.nii',
          '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S02/R_amygdala2example_func.nii')
)

S03 <- list(
  data=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S03/filtered_func_data_fMRI1.nii',
         '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S03/filtered_func_data_fMRI2.nii'),
  masks=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S03/L_amygdala2example_func.nii',
          '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S03/R_amygdala2example_func.nii')
)

S04 <- list(
  data=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S04/filtered_func_data_fMRI1.nii',
          '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Data/S04/filtered_func_data_fMRI2.nii'),
  masks=c('/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S04/L_amygdala2example_func.nii',
          '/Volumes/JOHNNYFIVE/Professional/Baycrest/S17/BrainHack/BrainHack_TO_2017/ExampleData/Masks/S04/R_amygdala2example_func.nii')
)


subj.list <- list(S01,S02,S03,S04)
names(subj.list) <- c("S01","S02","S03","S04")


data.list <- subject.data.list(subj.list)

## above is user script
## below to be function


# subject.data.list <- function(subj.list){
#
#   masked.subject.data <- vector("list",length(subj.list))
#   names(masked.subject.data) <- names(subj.list)
#
#   for(s in 1:length(subj.list)){
#     masked.subject.data[[s]] <- volsToMatrix(subj.list[[s]]$data, subj.list[[s]]$masks)
#   }
#
#  return(masked.subject.data)
#
# }




