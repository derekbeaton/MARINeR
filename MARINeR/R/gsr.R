## needs tests.

  ## x should be vectorized volumes (i.e., a matrix) -- TRs/volumes on the rows and vectorized voxels on the columns.
##allows for prespecified gsr values per volume (I don't know why, but fine) or median or mean as the predictor.
gsr <- function(x, gsr.vals = NA){
  
  if(gsr.vals=="median"){
    preds <- apply(x,1,median)
  }else if( length(gsr.vals) == nrow(x)){
    preds <- gsr.vals
  }
  else{
    preds <- apply(x,1,mean)
  }
  gsr.mat <- apply(x,2, 
    function(y){   
      resid( lm(y~preds) ) 
    }
  )
  rownames(gsr.mat) <- rownames(x)
  colnames(gsr.mat) <- colnames(x)
  return(gsr.mat)
}	