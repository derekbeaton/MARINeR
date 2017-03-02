

gsvd <- function(DAT,LW,RW, nu= min(dim(x)), nv = min(dim(x)), k = 0, tol=sqrt(.Machine$double.eps)){

  dat.for.svd <- sqrt.mat(LW) %*% DAT %*% sqrt.mat(RW)
  if(k<=0){
    k <- min(nrow(DAT),ncol(DAT))
  }
  res <- tolerance.svd(dat.for.svd,nu=nu,nv=nv,tol=tol)
  d <- res$d
  tau <- d^2/sum(d^2)
  comp.ret <- min(length(d),k)

  res$u <- res$u[,1:comp.ret]
  res$v <- res$v[,1:comp.ret]

  ## actually, I can make the sqrt & inv one special function for use here.
  P <- sqrt.mat(svd.pinv_sq(LW)) %*% res$u
  Q <- sqrt.mat(svd.pinv_sq(RW)) %*% res$v


  rownames(res$u) <- rownames(P) <- rownames(DAT)
  rownames(res$v) <- rownames(Q) <- colnames(DAT)

  return(list(p = P, q = Q, u=res$u, v=res$v, Dv = d[1:comp.ret], Dv.orig=d, tau = tau))

}
