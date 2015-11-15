roccurve<-function(score,class){
  # calculate roc curve
  # require the <caTools> R package

  require(caTools)  
  K=min(128,length(score))
  cut=seq(min(score)-0.000001,max(score)+0.000001,length=K)
  sensitivity=vector()
  specificity=vector()
  for (i in 1:K){
    result=sesp(score-cut[i],class)
    sensitivity[i]=result$sensitivity
    specificity[i]=result$specificity
  }
  AUC=trapz(specificity,sensitivity)
  
  return(list(sensitivity=sensitivity,specificity=specificity,AUC=AUC))
}
