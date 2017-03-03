rm(list=ls())
library(oro.nifti)

make.empty.brain <- function(nifti.image){
	return(array(0,dim=c(nifti.image@dim)))
}

plot.brain <- function(values,mask,which.voxels){
	empty.brain.r1 <- make.empty.brain(mask)
	empty.brain.r1[which.voxels] <- values
	dev.new()
	image(nifti(empty.brain.r1))
}

all.files <- list.files(pattern="*.nii")
wb.mask.add <- wb.mask <- readNIfTI('WholeBrain_mask.nii',reorient=FALSE)
brain.voxels <- which(wb.mask@.Data==1)

for(i in 1:length(all.files)){
	
	wb.mask.add@.Data <- wb.mask.add@.Data + eval(parse(text=paste(c("readNIfTI('",all.files[i],"',reorient=F)@.Data==1"),collapse="")))
	print(i)	
	
}

vis.mask <- (wb.mask.add@.Data > 2)
class(vis.mask) <- "numeric"
#image(wb.mask.add)


#plot.brain(vis.mask[brain.voxels],wb.mask,brain.voxels)
#plot.brain(wb.mask@.Data[brain.voxels],wb.mask,brain.voxels)


VV.mask <- wb.mask
VV.mask@.Data <- vis.mask
