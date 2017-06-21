linear.detred <- function(x){
						resid( lm( x~seq(x) ) )
					  }
					  
degree.detrend <- function(x,deg=1){
						preds <- seq(x)^deg
						resid( lm( x~ preds ) )
					  }					  
global.signal.regression <- function(x){
						resid( lm( x~mean(x) ) )
					  }	
svd.norm <- function(x,...){

	return(x/tolerance.svd(x,...)$d[1])
	
	## alternative:
		## this should probably, though it is wasteful, go through the full rebuild like below.
		## in that case, the $d is simply $d/$d[1]
		## that allows for some other flexibility.
	#res <- tolerance.svd(x,...)	
	#return( (res$u * matrix(res$d/res$d[1],nrow(res$u),ncol(res$u),byrow=T)) %*% t(res$v) )	
}
svd.low.rank.rebuild <- function(x,k=0,...){
	if(k<=0){
		k <- min(nrow(x),ncol(x))
	}
	res <- tolerance.svd(x,...)
	
	comp.ret <- 1:min(length(res$d),k)
	
	return( (res$u[,comp.ret] * matrix(res$d[comp.ret],nrow(res$u[,comp.ret]),ncol(res$u[,comp.ret]),byrow=T)) %*% t(res$v[,comp.ret]) )	
	
}

	## this is a highly specialized case, and we may not need it
gsvd.norm <- functiox(x,Wi,Wj){
	
}
	## this is a highly specialized case, and we may not need it
gsvd.low.rank.rebuild <- function(x){
	
}