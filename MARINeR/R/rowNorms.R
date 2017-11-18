rowNorms <- function(X,type=NULL,center=FALSE,scale=FALSE){
	if(is.null(type)){
		return(X)
	}else if(type=='hellinger'){
		return(sqrt(X/matrix(rowSums(X),nrow(X),ncol(X))))
	}else if(type == 'ca'){
		return(X/matrix(rowSums(X),nrow(X),ncol(X)))
	}else if (type == 'z'){
	  return(t(apply(X,1,scale,T,T)))
	}else if(type == 'other'){
			## this one is expensive.
		#return(t(expo.scale(t(X),center=center,scale=scale)))
		return(t(apply(X,1,scale,center,scale)))
	}else{
		return(X)
	}
}
