plotcarspls<-function(CARS){
#+++ CARS: An object returned by carspls.R
#+++ Hongdong Li, May.25, 2009 in R 2.8.1.
#+++ Contact: lhdcsu@gmail.com
#+++ Central South University in Changsha, P.R. China

Coef<-CARS$Coef
Nvar<-CARS$Nvar
RMSECV<-CARS$RMSECV
OPTiteration<-CARS$Optimal.iteration
M<-nrow(Coef)
P<-ncol(Coef)
layout(matrix(c(1,2,3),3))
#+++ Output 1
plot(Nvar,xlab="Number CARS-PLS iteration",ylab="Number of variables",
     pch=16,col="blue",cex.lab=1.6,cex.axis=1.6)
lines(Nvar,col="black")
abline(v=OPTiteration,lwd=2,lty=2,col="red")
#+++ Output 2
plot(RMSECV,xlab="Number CARS-PLS iteration",ylab="RMSECV",
     pch=16,col="blue",cex.lab=1.6,cex.axis=1.6)
lines(RMSECV,col="black")
abline(v=OPTiteration,lwd=2,lty=2,col="red")
#+++ Output 3
plot(Coef[1,],ylim=range(Coef),type="l",xlab="Number CARS-PLS iteration",
     ylab="coefficient path",col="blue",cex.lab=1.6,cex.axis=1.6)
for (i in 2:M){lines(Coef[i,],col="blue")}

abline(v=OPTiteration,lwd=2,lty=2,col="red")


}

#+++ END FOR PLOT.CARSPLS
#+++  Nature with rain.
#+++ There is a song you like to sing.