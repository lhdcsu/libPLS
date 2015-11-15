carsplsorigin<-function(X,y,nLV=2,fold=10,ratio=0.9,scale.pretreat=0,iteration=50,PartitionType="interleaved")
{
#+++ CARS: Competitive Adaptive Reweighted Sampling method for
#+++ X: Sample matrix, sample in row and variable in column.
#+++ y: The response value.
#+++ nLV:   the number of latent variables in PLS.
#+++ fold: The number of segments for cross validation, defalut 5.
#+++ scale.pretreat: Whether divided by the standard deviation for each variable.
#+++              1: scale
#+++              0: no scale(only centered)
#+++ ratio: the ratio of sampled samples in CARS. 
#+++ iteration: the number of Monte Carlo samplings in CARS.
#+++ Supervisor: Yizeng Liang, yizeng_liang@263.net.
#+++ Hongdong Li, May.25, 2009 in R 2.8.1.
#+++ Contact: lhdcsu@gmail.com
#+++ Central South University in Changsha, P.R. China



#+++ Initial settings
Num_row<-nrow(X)
Num_col<-ncol(X)       
data.CARS<-data.frame(indepdent=X,response=y)  #+++ Convert into dataframe
RMSECV<-rep(0,iteration)
NumLV<-rep(0,iteration)
VarIndex<-1:Num_col

#+++ Some predifined data for CARS
subsetVariable<-1:Num_col     #+++ selected variables for PLS modelling
Nsample<-as.integer(Num_row*ratio)  #+++ Number of samples in Monte Carlo Sampling
Coef<-matrix(rep(0,Num_col*iteration),Num_col) #+++ Coefficient matrix in CARS
Nvar<-rep(0,iteration)
                                                                                
#+++ Parameter of exponentially decreasing function.
ratio0<-1
ratio1<-2/Num_col
b=log(ratio0/ratio1)/(iteration-1)
a=ratio0*exp(b)

#+++ Main Loop for CARS Algorithm
for (iter in 1:iteration){
     #+++ Monte Carlo Sampling
     subseti<-sample(1:Num_row,Nsample,replace=FALSE)
     Xcal<-X[subseti,subsetVariable]
     ycal<-y[subseti]                           
     nLV<-min(c(nLV,dim(Xcal)))
     #+++ PLS modeling 
     data.CARS.cal<-data.frame(indepdent=Xcal,response=ycal)  #+++ Convert into dataframe    
     ncomp=nLV
     if (scale.pretreat==1) {plsr.fit<- mvr(response~.,ncomp,data = data.CARS.cal,method = "simpls",scale=TRUE)}
     else{plsr.fit<- mvr(response~.,ncomp,data = data.CARS.cal,method = "simpls",scale=FALSE)}
     #+++ Model selection by cross validation
     CV<-crossval(plsr.fit, segments =fold,data=data.CARS.cal,segment.type =PartitionType)
     validation<-CV$validation
     PRESS<-validation$PRESS
     RMSECV.temp=sqrt(PRESS/Num_row)
     RMSECV[iter]<-min(RMSECV.temp)
     NumLV[iter]<-which.min(RMSECV.temp)
     
     
     #+++ Extract the coefficients and store them into the matrix: Coef. 
     coef0<-rep(0,Num_col)
     coef.iter<-plsr.fit$coefficients[,,nLV]
     coef0[subsetVariable]<-coef.iter
     Coef[,iter]<-coef0
     
     #+++ Weights of each variable
     weight<-abs(coef0)
     weight.order<-order(weight,decreasing=TRUE)
     
     #+++ Calculate the ratio of variables to be retained by EDF in CARS.
     ratioVariable<-a*exp(-b*iter)
     Nvar[iter]<-length(which(coef0!=0))
     K<-as.integer(Num_col*ratioVariable)
     
     #+++ Eliminate the variables of small regression coefficients by force.
     weight[weight.order[K+1:Num_col]]<-0
     
     #+++ Adaptive Reweighted Sampling
     subsetVariable<-sample(VarIndex,Num_col,replace=TRUE,prob=weight)
     subsetVariable<-unique(subsetVariable)  
     
     #+++ screen print
     screen.output<-paste("The",iter,"th CARS-PLS iteration finished.")
     print(screen.output)
}

OPT.iter=which.min(RMSECV)
min.RMSECV=min(RMSECV)
CARS<-list(Coef=Coef,Nvar=Nvar,RMSECV=RMSECV,
      NumLV=NumLV,Optimal.iteration=OPT.iter,min.RMSECV=min.RMSECV,
      selected.variables=which(Coef[,OPT.iter]!=0))
return(CARS)  #+++ Returned list data for CARS program

}         #+++ END for function
 

 #+++ END END END END END END. Put your heart into it.
 #+++ I know where I am.
 
 
 