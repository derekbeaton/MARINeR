### needs tests.

svd.low.rank.rebuild <- function(x, rank.rebuild = 0, ...){
  
  if( length(rank.rebuild) > 1 ){
    comp.ret <- unique( ifelse(rank.rebuild < 1, 1, rank.rebuild) )
    res <- gsvd(x,k = max(comp.ret))
    return( (res$u[,comp.ret] * matrix(res$d[comp.ret],nrow(res$u[,comp.ret]),ncol(res$u[,comp.ret]),byrow=T)) %*% t(res$v[,comp.ret]) )
  }else if( length(rank.rebuild) == 1 ){
    if(rank.rebuild > 0 & rank.rebuild < 1){
      res <- gsvd(x)
      comp.ret <- 1:max(which( cumsum(res$tau) < rank.rebuild))
      return( (res$u[,comp.ret] * matrix(res$d[comp.ret],nrow(res$u[,comp.ret]),ncol(res$u[,comp.ret]),byrow=T)) %*% t(res$v[,comp.ret]) )	
    }else{
      res <- gsvd(x, k = ifelse(rank.rebuild < 0, 0, round(rank.rebuild)) )
      return( (res$u * matrix(res$d,nrow(res$u),ncol(res$u),byrow=T)) %*% t(res$v) )
    }
  }else{
    res <- gsvd(x)
    comp.ret <- 1:min( max(which( cumsum(res$tau) < .9)), 10 )
    return( (res$u[,comp.ret] * matrix(res$d[comp.ret],nrow(res$u[,comp.ret]),ncol(res$u[,comp.ret]),byrow=T)) %*% t(res$v[,comp.ret]) )	
  }
  
}