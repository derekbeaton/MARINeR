library(oro.nifti)
library(TExPosition)


make.empty.brain <- function(nifti.image){
	return(array(0,dim=c(nifti.image@dim)))
}


plot.timecourse <- function(data.in,columns=NA){
	
	if(is.na(columns) || (length(columns) < 3)){
		columns <- c(.1,.25,.5,.75,.9)*ncol(data.in)
	}
	
	these.colors <- createColorVectorsByDesign(diag(length(columns)))
	column.summary <- summary(c(data.in[,columns]))
	ymax <- column.summary[6]
	ymin <- min(0,column.summary[1])
	##pick a few random voxels and plot time courses:
	dev.new()
	plot(1:nrow(data.in),data.in[,columns[1]],
		cex=2,col=c(rep(these.colors$gc[1,],nrow(data.in))),
		type="l",
		xlim=c(-1,nrow(data.in)+2),ylim=c(ymin,ymax))
	
	for(i in 2:length(columns)){
		points(1:nrow(data.in),data.in[,columns[i]],
		cex=2,col=c(rep(these.colors$gc[i,],nrow(data.in))),type="l")
	}
	
}

plot.brain <- function(values,mask,which.voxels){
	empty.brain.r1 <- make.empty.brain(mask)
	empty.brain.r1[which.voxels] <- values
	dev.new()
	image(nifti(empty.brain.r1))
}

lin.det <- function(x){
						resid( lm( x~seq(x) ) )
					  }
gsr <- function(x){
						print(mean(x))
						resid( lm( x~mean(x) ) )
					  }					  

###Make a VV mask
all.files <- paste0("../OtherData/Masks/",list.files('../OtherData/Masks/',pattern="*.nii"))
wb.mask.add <- wb.mask <- readNIfTI('../OtherData/Masks/WholeBrain_mask.nii',reorient=FALSE)
brain.voxels <- which(wb.mask@.Data==1)

for(i in 1:length(all.files)){
	
	wb.mask.add@.Data <- wb.mask.add@.Data + eval(parse(text=paste(c("readNIfTI('",all.files[i],"',reorient=F)@.Data==1"),collapse="")))

	
}

vis.mask <- (wb.mask.add@.Data > 2)
class(vis.mask) <- "numeric"

VV.mask <- wb.mask
VV.mask@.Data <- vis.mask
