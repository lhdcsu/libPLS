classplot2<-function(x1,x2,class,xlabel="PC 1", ylabel="PC 2"){

uc=unique(class)  
k0=which(class==uc[1])
k1=which(class==uc[2])
c1=as.character(uc[1])
c2=as.character(uc[2])
plot(seq(min(x1),max(x1),length.out=10),seq(min(x2),max(x2),length.out=10),type="n",xlab=xlabel,ylab=ylabel)
points(x1[k0],x2[k0],col="blue",pch=15)
points(x1[k1],x2[k1],col="red",pch=16)
legend("topright",c(c1,c2),pch=c(15,16),col=c("blue","red"))

}

