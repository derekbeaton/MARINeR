my.svd <- function(x,tol=.Machine$double.eps*100){	## consider increasing the tolerance.
	
	svd.res <- svd(x)
	comps.to.keep <- which(!(svd.res$d^2 < tol))
		
	svd.res$u <- as.matrix(svd.res$u[,comps.to.keep])
	rownames(svd.res$u) <- rownames(x)
	svd.res$v <- as.matrix(svd.res$v[,comps.to.keep])
	rownames(svd.res$v) <- colnames(x)	
	svd.res$d <- svd.res$d[comps.to.keep]
	
	svd.res$u[ abs(svd.res$u) < tol ] <- 0
	svd.res$v[ abs(svd.res$v) < tol ] <- 0	
	
	return(svd.res)
	
}

gsvd <- function(G.DAT,LW,RW,k=0,tol=sqrt(.Machine$double.eps)){
	
	dat.for.svd <- sqrt.mat(LW) %*% G.DAT %*% sqrt.mat(RW)
	if(k<=0){
		k <- min(nrow(G.DAT),ncol(G.DAT))
	}	
	res <- my.svd(dat.for.svd,tol=tol)
	d <- res$d
	tau <- d^2/sum(d^2)
	comp.ret <- min(length(d),k)
	
	res$u <- res$u[,1:comp.ret]
	res$v <- res$v[,1:comp.ret]

		## actually, I can make the sqrt & inv one special function for use here.
	P <- sqrt.mat(svd.pinv_sq(LW)) %*% res$u
	Q <- sqrt.mat(svd.pinv_sq(RW)) %*% res$v
	
	
	rownames(res$u) <- rownames(P) <- rownames(G.DAT)		
	rownames(res$v) <- rownames(Q) <- colnames(G.DAT)	
	
    return(list(p = P, q = Q, u=res$u, v=res$v, Dv = d[1:comp.ret], Dv.orig=d, tau = tau))
}


###this could be sped up at the bottom.
sqrt.mat <- function(X,tol=sqrt(.Machine$double.eps)){
	
	isDiagonal.matrix <- function(X){
		if(is.null(dim(X))){
			stop("sqrt.mat: X is not a matrix.")
		}
		return(all(X[lower.tri(X)] == 0, X[upper.tri(X)] == 0))
	}
	
	##first, test if it is symmetric in size; and other symmetric properties.
	if(!isSymmetric.matrix(X,tol=tol)){
		warning("sqrt.mat: Weight/Mass matrix is not symmetric; using SVD for sqrt of rectangular matrix.")
		return(svd.sqrt.mat(X))
		###I can just do the SVD one here...
	}
			
	##test if it is a diagonal matrix -- then just sqrt it and send back.
	if(isDiagonal.matrix(X)){
		return( diag(sqrt(diag(X))) )
	}else{
		## I think this is where I can get speed up. Also... the eigen is not necessarily safe for this.		
		## the only advantage here is to stop() if eigenvalues are complex or less than 0.
			## alternatively, I should just drop them (but sometimes those are important things to note)
		A <- eigen(X)
		##test for complex values:
		if(any(unlist(lapply(A$values,is.complex)))){
			stop("sqrt.mat: Weight/Mass matrix not positive definitive. Eigenvalues are complex.")
		}
		
		##change values below tolerance		
		A$values[which(A$values < tol)] <- 0

		##first, test if positive definite
		if( sum(A$values < 0 )>0 ){
			stop("sqrt.mat: Weight/Mass matrix not positive definite. Some eigenvalues are less than 0")	
		}else{		
			return((A$vectors * matrix(sqrt(A$values),nrow(A$vectors),ncol(A$vectors),byrow=T)) %*% t(A$vectors))
		}
	}
}

##an alternative square root function for rectangular matrices (if needed).
svd.sqrt.mat <- function(X,tol=sqrt(.Machine$double.eps)){
	res <- my.svd(X,tol=tol)
	return( (res$u * matrix(sqrt(res$d),nrow(res$u),ncol(res$u),byrow=T)) %*% t(res$v) )
}

###not using this as of now, but can later.
svd.pinv <- function(X,tol=sqrt(.Machine$double.eps)){
	res <- my.svd(X,tol=tol)
	return( (res$v * matrix(1/res$d,nrow(res$v),ncol(res$v),byrow=T)) %*% t(res$u) )
}

###Am using this in place of ginv() for square symmetric matrices. ginv() returns some rounding errors and makes the eigenvalues imaginary. 
svd.pinv_sq <- function(X,tol=sqrt(.Machine$double.eps)){
	
	###I am requiring this to be square.
	if( !isSymmetric.matrix(X, tol= tol) ){
		stop("svd.pinv_sq: Matrix not symmetric")
	}
	res <- my.svd(X,tol=tol)	
	return( (res$u * matrix(1/res$d,nrow(res$u),ncol(res$u),byrow=T)) %*% t(res$u) )
}



