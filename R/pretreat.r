pretreat<-function(X,method){
  #+++ pretreat data
  # X: data matrix to pretreat
  # method:   "center", or "scaling", or "minmax"
  #         or  a 2xp matrix with row 1 to substract and row 2 to divide
  # HDLi, Jun. 2015 @Queen Anne
  
  
  if (is.vector(X)) {X=matrix(X,ncol=1)}
  n=nrow(X)
  p=ncol(X)
  
  
  if (is.matrix(method)){
    a=method[1,]
    b=method[2,]
  }else{
    if (method == "center"){
     a=apply(X,2,mean)
      b=rep(1,length=p)
    }else if (method=="scaling"){
      a=apply(X,2,mean)
      b=apply(X,2,sd)
    
    }else if (method=="minmax"){
      a=apply(X,2,min)
      tmp=apply(X,2,range)
      b=abs(tmp[1,]-tmp[2,])
    }
  }
  
  for (j in 1:p){
    X[,j]=(X[,j]-a[j])/b[j]
  }
  return(list(X=X,a=a,b=b))  

}
