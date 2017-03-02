rm(list=ls())
gc()
source('0_Utilities.R')

# load('../Cleaned/data.mat.r1.a.norm.rda')
# load('../Cleaned/data.mat.r1.orig.norm.rda')

# load('../Cleaned/data.mat.r1.a.rda')
# load('../Cleaned/data.mat.r1.orig.rda')

# load('../Cleaned/brain.voxels.keep.rda')
# load('../Cleaned/brain.voxels.rda')

load('../Cleaned/data.mat.r1.vv.norm.rda')

DES <- read.csv('../Data/DES.csv',header=T,row.names=1)


#a<-epPCA(data.mat.r1.a.norm,DESIGN=DES,make_design_nominal=FALSE,scale=FALSE,center=FALSE,graphs=FALSE)
#a<-epPCA(data.mat.r1.vv.norm,DESIGN=DES,make_design_nominal=FALSE,scale=FALSE,center=FALSE,graphs=FALSE)
#a<-epPCA(data.mat.r1.wb.norm,DESIGN=DES,make_design_nominal=FALSE,scale=FALSE,center=FALSE,graphs=FALSE)

a<-epPCA(data.mat.r1.vv.norm,DESIGN=DES,make_design_nominal=FALSE,scale=T,center=T,graphs=FALSE)

a<-tepBADA(data.mat.r1.vv.norm,DESIGN=DES,make_design_nominal=FALSE,scale=FALSE,center=FALSE,graphs=FALSE)

prettyScree(a$ExPosition.Data$eigs)


xa <- 1
ya <- 2
##real quick
colz <- createColorVectorsByDesign(DES)
prettyPlot(a$TExPosition.Data$fi,col=a$Plotting.Data$fi.col,display_names=FALSE,x_axis=xa,y_axis=ya)

prettyPlot(a$TExPosition.Data$fi,col=a$Plotting.Data$fi.col,display_names=FALSE,x_axis=xa,y_axis=ya)
for(i in 1:ncol(DES)){
	peeledHull(a$TExPosition.Data$fi[which(DES[,i]==1),],col=colz$gc[i],x_axis=xa,y_axis=ya)
}

prettyPlot(a$TExPosition.Data$fi,col=a$Plotting.Data$fi.col,display_names=FALSE,x_axis=xa,y_axis=ya)
for(i in 1:ncol(DES)){
	this.thing <- t(colMeans(a$TExPosition.Data$fi[which(DES[,i]==1),]))
	rownames(this.thing) <- rownames(colz$gc)[i]
	prettyPlot(this.thing,col=colz$gc[i],dev.new=FALSE,axes=FALSE,new.plot=FALSE,pch=15,cex=3,x_axis=xa,y_axis=ya)
}




full.brain <- array(0,dim=c(64,64,31))
for.brain <- a$TExPosition.Data$fj[,3]-min(a$TExPosition.Data$fj[,3])

full.brain[brain.voxels.keep] <- for.brain


for.nif <- nifti(full.brain,datatype=16,intent_code=11,intent_p1=mean(for.brain),intent_p2=sd(for.brain))
image(for.nif,col=hotmetal(64))

#epPCA(data.mat.r1.orig.norm,make_design_nominal=FALSE,scale=FALSE,center=FALSE)