pls<-function(X,Y,A){
  #+++ Partial Least Squares for prediction, the NIPALS algorithm for PLS-2
  #   So Y can be a vector or a matrix of multiple responses
  #+++ HDLi,lhdcsu@gmail.com
  #+++ pls calls the basic function: plsnipals.r
  # Jun. 25, 2015
  
  Xp=pretreat(X,"center")
  Yp=pretreat(Y,"center")
  X1=Xp$X
  Y1=Yp$X
  A=min(c(A,min(dim(X))))
  
  
  model=plsnipals(X1,Y1,A)
  coef_all=model$coef_all
  intercept_all=vector()
  for (i in 1:A){
    intercept_all[i]=Yp$a-sum(Xp$a*coef_all[,i])
  }
  
  
  Yfit=X%*%coef_all[,A]+intercept_all[A]
  R2=1-sum((Y-Yfit)^2)/sum(Y1^2)
  return(list(coef=matrix(coef_all[,A],ncol=1),intercept=intercept_all[A],Yfit=Yfit,R2=R2,coef_all=coef_all,intercept_all=intercept_all))
}


