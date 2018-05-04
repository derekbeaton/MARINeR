## data.norms tests -- I hope this can be one function to rule them all.

library(ExPosition)

  ## I could steal the approach used by expo.scale to force a center & scale to exist.
data.norms <- function(X,type=NULL,center=F,scale=F,margin=2){

  orig.dims <- dim(X)
  orig.names <- dimnames(X)

  if(is.null(type) & (center==F | is.na(center) | is.null(center)) & (scale==F | is.na(scale) | is.null(scale))){
    return(X)
  }else if(type=="ca"){
    X <- apply(X,margin,function(x){ x/sum(x) })
  }else if(type=="hellinger"){
    X <- apply(X,margin,function(x){ sqrt(x/sum(x)) })
  }else if(type == "z"){
    X <- apply(X,margin,function(x){scale(x,center=T,scale=T)})
  }else if(type=="ss1"){
    X <- apply(X,margin,function(x){scale(x,center=T,scale=sd(x))}) / sqrt(orig.dims[-margin]-1)
  }else if(type=="scale"){
    X <- apply(X,margin,function(x){scale(x,center=center,scale=scale)})
  }else{
    return(X)
  }

 new.dims <- dim(X)
 if(new.dims[1]==orig.dims[2] & new.dims[2]==orig.dims[1]){
   X <- t(X)
 }
 dimnames(X) <- orig.names
 return(X)
}

data("beer.tasting.notes")
test.dat <- beer.tasting.notes$data


scale(test.dat) / data.norms(test.dat,type="z")
expo.scale(test.dat,scale="SS1") / data.norms(test.dat,type="ss1")
t(expo.scale(t(test.dat),scale="SS1")) / data.norms(test.dat,margin = 1,type="ss1")


rowNorms(test.dat,type="ca") / data.norms(test.dat,margin = 1,type="ca")
rowNorms(test.dat,type="hellinger") / data.norms(test.dat,margin = 1,type="hellinger")
rowNorms(test.dat,type="z") / data.norms(test.dat,margin = 1,type="z")


t(rowNorms(t(test.dat),type="ca")) / data.norms(test.dat,margin = 2,type="ca")
t(rowNorms(t(test.dat),type="hellinger")) / data.norms(test.dat,margin = 2,type="hellinger")
t(rowNorms(t(test.dat),type="z")) / data.norms(test.dat,margin = 2,type="z")



