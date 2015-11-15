plsnipals<-function(X,Y,A){
  #+++ Partial Least Squares, the NIPALS algorithm for PLS-2
  #   So Y can be a vector or a matrix of multiple responses
  #+++ HDLi,lhdcsu@gmail.com
  # Jun. 25, 2015
  
  varX=sum(sum(X^2))
  varY=sum(sum(Y^2))
  A=min(c(A,min(dim(X))))
  n=nrow(X)
  p=ncol(X)
  py=ncol(Y)
  
  W=matrix(NA,nrow=p, ncol=A)
  T=matrix(NA,nrow=n, ncol=A)
  P=matrix(NA,nrow=p, ncol=A)
  B=matrix(NA,nrow=p, ncol=A)
  Q=matrix(NA,nrow=py, ncol=A)
  
  
  for (i in 1:A){
    error=1
    u=matrix(Y[,1],ncol=1)
    niter=0
    while (error > 1e-8 && niter < 1000){
      w=(t(X)%*%u)/sum(u^2)
      w=w/sqrt(sum(w^2))
      t=X%*%w
      q=t(Y)%*%t/sum(t^2)
      u1=Y%*%q/sum(q^2)
      error=sum((u1-u)^2)/sum(u^2)
      u=u1
      niter=niter+1
    }
    p=t(X)%*%t/sum(t^2)
    X=X-t%*%t(p)
    Y=Y-t%*%t(q)
    
    #+++ store
    W[,i]=w
    T[,i]=t
    P[,i]=p
    Q[,i]=q
    
  }
  #+++ calculate explained variance
  R2X=diag(t(T)%*%T%*%t(P)%*%P)/varX
  R2Y=diag(t(T)%*%T%*%t(Q)%*%Q)/varY
  Wstar=W%*%solve(t(P)%*%W)
  
  # calculate regression coefficients
  for (i in 1:A){
    B[,i]=Wstar[,1:i]%*%matrix(Q[,1:i],ncol=1)
  }
  
  
  #+++ return
  return(list(coef=matrix(B[,A],ncol=1),coef_all=B,Wstar=Wstar,W=W,T=T,P=P,W=W,R2X=R2X,R2Y=R2Y))
}
