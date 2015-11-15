plsval<-function(model,Xnew){
  #+++ Partial Least Squares for prediction, the NIPALS algorithm for PLS-2
  #   So Y can be a vector or a matrix of multiple responses
  #+++ HDLi,lhdcsu@gmail.com
  #+++ model: returned by plsnipals.r
  # Jun. 25, 2015
  
  Ypred=Xnew%*%model$coef+model$intercept
  
  #+++ return
  return(Ypred)
}
