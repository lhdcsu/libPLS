plslda<-function(X,Y,A=2){
  #+++ Partial Least Squares-Discriminant Analysis based on PLS regression
  #+++  X: Input matrix  n x p
  #+++  Y: class label   n x 1, has to be 1 or -1
  #+++ HDLi,lhdcsu@gmail.com
  #+++ pls calls the basic function: plsnipals.r
  # Jun. 25, 2015
  
  
  Y[Y==0]=-1
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
  YfitLabel=sign(Yfit)
  errorRate=1-sum(Y==YfitLabel)/nrow(X)
  roc=roccurve(Yfit,Y)
  return(list(coef_all=coef_all,intercept_all=intercept_all,coef=matrix(coef_all[,A],ncol=1),intercept=intercept_all[A],Yfit=Yfit,YfitLabel=YfitLabel,errorRate=errorRate,AUC=roc$AUC,sensitivity=roc$sensitivity,specificity=roc$specificit))

}


