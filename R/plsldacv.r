plsldacv<-function(X,y,A=3,K=5){
  #+++ PLS-LDA k-fold cross validation
  # y has to be 1 or -1
  # if y is 0 or 1, then 0 will be replace with -1
  
  y[y==0]=-1
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
    PLS=plslda(Xcal,ycal,A)
    
    # predict
    for (i in 1:A){  
      b=matrix(PLS$coef_all[,i],ncol=1)
      ypred=Xval%*%b+PLS$intercept_all[i]
      pred[kval,i]=ypred
    }
  }
  
  # calculate RMSECV: Root Mean Squared Errors of Cross Validation
  errorRate=vector()
  predClass=sign(pred)
  for (i in 1:A){
    errorRate[i]=1-sum(predClass[,i]==y)/n 
  }
  optLV=which.min(errorRate)
  
  # return
  return(list(predClass=predClass,errorRate=errorRate,optLV=optLV))
}


