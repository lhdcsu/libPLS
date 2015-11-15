plotmcs<-function(MCS,mark=FALSE){
  # MCS, an object returned by mcs.r
  
  
  delta=(max(MCS$MEAN)-min(MCS$MEAN))/40
  plot(MCS$MEAN,MCS$SD,pch=20,col="green",xlab="MEAN",ylab="SD")
  if (mark==TRUE){
    out=MCS$outlier
    text(MCS$MEAN[out]+delta,MCS$SD[out],labels=out,col="gray50",cex=0.5)
  }
}
