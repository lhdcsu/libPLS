mcs<-function(X,Y,A=2,N=1000,ratio=0.75,alpha=2){
  #+++ MCS(Monte Carlo Sampling) method for detecting outliers
  #+++ HDLi,lhdcsu@gmail.com
  # Jun. 25, 2015
  
  n=nrow(X)
  p=ncol(Y)
  nsub=floor(n*ratio)
  error=matrix(NA,nrow=N,ncol=n)
  for (i in 1:N){
    perm=sample(n,n)
    kcal=perm[1:nsub]
    kval=perm[-c(1:nsub)]
    model=pls(X[kcal,],Y[kcal],A)
    ypred=plsval(model,X[kval,])
    e=ypred-Y[kval]
    error[i,kval]=e
  }
  
  
  # calculate MEAN and SD
  MEAN=vector()
  SD=vector()
  for (i in 1:n){
    kgood=!is.na(error[,i])
    ei=error[kgood,i]
    MEAN[i]=abs(mean(ei))
    SD[i]=sd(ei)
  }
  
  # decide outliers
  nrb=round(n*0.95)
  
  rank=order(MEAN)[1:nrb]
  cutMEAN=mean(MEAN[rank])+alpha*sd(MEAN[rank])
  
  rank=order(SD)[1:nrb]
  cutSD=mean(SD[rank])+alpha*sd(SD[rank])
  
  outlier=union(which(MEAN>cutMEAN),which(SD>cutSD))
  keep=1:n
  keep=keep[-outlier]
  
  # return
  return(list(error=error,MEAN=MEAN,SD=SD,keep=keep,outlier=outlier,cutMEAN=cutMEAN,cutSD=cutSD))
  

} # end of function



