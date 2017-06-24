### needs tests.

svd.low.rank.rebuild <- function(x, rank.rebuild = 0, ...){
  
  if( length(rank.rebuild) > 1 ){
    comp.ret <- unique( ifelse(rank.rebuild < 1, 1, rank.rebuild) )
    res <- gsvd(x,k = max(comp.ret))
    return( (res$u[,comp.ret] * matrix(res$d[comp.ret],nrow(res$u[,comp.ret]),ncol(res$u[,comp.ret]),byrow=T)) %*% t(res$v[,comp.ret]) )
  }else if( length(rank.rebuild) == 1 & !is.na(rank.rebuild) ){
    if(rank.rebuild > 0 & rank.rebuild < 1 ){
      res <- gsvd(x)
      comp.ret <- 1:(max(which( cumsum(res$tau) <= rank.rebuild))+1)
      return( (res$u[,comp.ret] * matrix(res$d[comp.ret],nrow(res$u[,comp.ret]),ncol(res$u[,comp.ret]),byrow=T)) %*% t(res$v[,comp.ret]) )	
    }else if( (rank.rebuild > 0) ){
      res <- gsvd(x, k = round(rank.rebuild) )
      return( (res$u * matrix(res$d,nrow(res$u),ncol(res$u),byrow=T)) %*% t(res$v) )
    }else{
        ## there can be an error here when cumsum(tau) has > .9 as the first component...
      res <- gsvd(x)
      comp.ret <- 1:min( max(which( cumsum(res$tau) < .9))+1, 10 )
      return( (res$u[,comp.ret] * matrix(res$d[comp.ret],nrow(res$u[,comp.ret]),ncol(res$u[,comp.ret]),byrow=T)) %*% t(res$v[,comp.ret]) )	
    }
  }else{  ## go here for stupid or empty values? that means all before need to be better tests.
    ## there can be an error here when cumsum(tau) has > .9 as the first component...
    res <- gsvd(x)
    comp.ret <- 1:min( max(which( cumsum(res$tau) < .9))+1, 10 )
    return( (res$u[,comp.ret] * matrix(res$d[comp.ret],nrow(res$u[,comp.ret]),ncol(res$u[,comp.ret]),byrow=T)) %*% t(res$v[,comp.ret]) )	
  }
  
}