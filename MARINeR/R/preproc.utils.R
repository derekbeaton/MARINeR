## preprocessing & utilities

svd.norm <- function(x){

  return(x/tolerance.svd(x)$d[1])

}

snr <- function(DATA){

  return(apply(DATA,2,function(x){sqrt(mean(x)/sd(x))}))

}

degree.detrend <- function(x,deg=1){

  deg.mat <- resid( lm( x ~ poly(1:nrow(x), degree = deg)) )
  rownames(deg.mat) <- rownames(x)
  colnames(deg.mat) <- colnames(x)
  return( deg.mat )

}


## I think I should replace row.norms and expo.scale with some more general purpose approach
  ## so I think it would be best to just limit this to profiles, center, scale via scale()
  ## then I can pick the margin (i.e., 1 vs. 2)
  ## and this would perform a sweep() or an apply()

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
