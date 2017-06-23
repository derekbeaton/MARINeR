## needs tests.

  ## x should be vectorized volumes (i.e., a matrix) -- TRs/volumes on the rows and vectorized voxels on the columns.
##allows for prespecified gsr values per volume (I don't know why, but fine) or median or mean as the predictor.
gsr <- function(x, gsr.vals = NA){
  
  if( length(gsr.vals) == nrow(x)){
    preds <- gsr.vals
  }else if( length(gsr.vals) == 1 ){
    if( is.na(gsr.vals) ){
      preds <- apply(x,1,mean)
    }else if(gsr.vals=="median"){
      preds <- apply(x,1,median)
    }else{ ## I don't know what you want from me!
      preds <- apply(x,1,mean)
    }
  }
  
    ## I may not have to apply...
  # gsr.mat <- apply(x,2, 
  #   function(y){   
  #     resid( lm(y~preds) ) 
  #   }
  # )
  
  gsr.mat <- resid( lm(x~preds, na.action = na.exclude) )
  rownames(gsr.mat) <- rownames(x)
  colnames(gsr.mat) <- colnames(x)
  return(gsr.mat)
}	