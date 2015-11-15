plscv<-function(X,y,A=3,K=5){
  #+++ PLS k-fold cross validation
  
  
  sort=order(y)
  X=X[sort,]
  y=y[sort]
  
  n=nrow(X)
  p=ncol(X)
  A=min(c(A,n,p))
  group=(c(1:n)-1)%%K+1
  
  pred=matrix(NA,nrow=n,ncol=A)
  for (k in 1:K){
    kcal=which(group!=k)
    kval=which(group==k)
    Xcal=X[kcal,]
    ycal=y[kcal]
    
    Xval=X[kval,]
    yval=y[kval]
    
    
    # build a model
    PLS=pls(Xcal,ycal,A)
    
    # predict
    for (i in 1:A){  
      b=matrix(PLS$coef_all[,i],ncol=1)
      ypred=Xval%*%b+PLS$intercept_all[i]
      pred[kval,i]=ypred
    }
  }
  
  # calculate RMSECV: Root Mean Squared Errors of Cross Validation
  RMSECV=vector()
  error=pred-y
  for (i in 1:A){
    RMSECV[i]=sqrt(sum(error[,i]^2)/n)  
  }
  optLV=which.min(RMSECV)
  
  # return
  return(list(predError=error,RMSECV=RMSECV,optLV=optLV))
}


