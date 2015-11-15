plsldaval<-function(model,Xnew){
  #+++ Make predictions on new samples Xnew using a PLS-LDA model
  #+++ HDLi,lhdcsu@gmail.com
  #+++ model: returned by plsnipals.r
  # Jun. 25, 2015
  
  Ypred=Xnew%*%model$coef+model$intercept
  YpredLabel=sign(Ypred)
  #+++ return
  return(list(Ypred=Ypred,YpredLabel=YpredLabel))
}
