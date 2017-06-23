## this will need to eventually be the GSVD norm
  ## the safest version is probably the rebuild... but I need to check the French literature to see if I can just divide by the first SVD SV.
svd.norm <- function(x){
  
  return(x/tolerance.svd(x)$d[1])
  
  ## alternative:
  ## this should probably, though it is wasteful, go through the full rebuild like below.
  ## in that case, the $d is simply $d/$d[1]
  ## that allows for some other flexibility.
  #res <- tolerance.svd(x,...)	
  #return( (res$u * matrix(res$d/res$d[1],nrow(res$u),ncol(res$u),byrow=T)) %*% t(res$v) )	
  
}