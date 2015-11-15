spa<-function(X,y,nLV=2,Q=3,Nmcs=1000,ratio=0.75,fold=3)
{
#+++ Subwindow Permutation Analysis for variable importance assessment.
#+++ Input:  X: m x p  (Sample matrix)
#            y: m x 1  (measured property), note that only 1 or -1 is allowed in y.
#         fold: Number of fold for cross validation.
#          nLV: The allowed maximal number of PLS components for cross-validation
#         Nmcs: The number of Monte Carlo Simulation
#        ratio: The ratio of calibration samples to the total samples.
#            Q: The number of variables to be sampled in each MCS.
#      scale.pretreat: whether scaled by the standard deviation.
#                      TRUE: autoscaling
#                      FALSE: only centered
#          OPT: =1 : nonparametric test, default.
#               =0 : t test.


#+++ Output: Structural data: F with items:
#          optLV: The optimal number of PLS components for each submodel.
#       error0: The normal prediction errors of the N submodels.
#       error1: The permutation prediction errors of the N submodels
#     interfer: a vector of size p. '1' indicates the variable is interferring
#           p: the p-value of each variable resulting from SPA. Note that
#               if a variable is interfering, the its p is manually added by 1, that is p=1+p.
#               This means that the variable of p>1 is an interferring one. It should be removed
#               when building a classification model.
#         COSS: A metric for evaluating the importance of each variable,
#              COSS=-ln(p). According to the calculation of p,an
#              interferring variable will have a minus COSS score. This is
#               a trick.
#+++ Rankedvariable: Ranked list of vairables.
#+++ Hongdong Li, Oct. 16, 2008.
#+++ Revised in Nov.23, 2009.
#+++ Revised in Jun.26, 2015.
#+++ Advisor: Yizeng Liang, yizeng_liang@263.net.
#+++ Coded by Hongdong Li, March.23, 2010 in R 2.10.1.
#+++ Contact: lhdcsu@gmail.com
#+++ Central South University in Changsha, P.R. China
#+++ Reference:




#+++ Initial settings
Num_row<-nrow(X)
Num_col<-ncol(X)
optLV<-rep(0,Nmcs)
error0<-rep(0,Nmcs)
error1<-matrix(NA,nrow=Nmcs,ncol=Num_col)
p<-rep(NA,Num_col)          # p-value for each variable
COSS<-rep(0,Num_col)        # COnditional Synergetic Score: COSS
interfer<-rep(0,Num_col)    # Interferring vairables indicator

nLV<-min(c(nLV,Num_row,Num_col,Q))    # nLV revised
Qs<-round(Num_row*ratio)
Qv=Num_row-Qs


if (Q>=Num_col) {Q<-round(2*Num_col/3)+1   #  Correction
screen.output<-paste("Q is larger than the number of variables!")
print(screen.output) }


VariableIndex<-1:Num_col
SampleIndex<-1:Num_row

#+++ Main Loop for SPA Algorithm
for (iter in 1:Nmcs){
     #+++ sub-dataset based on Monte Carlo Sampling in both sample and variable space
     
     subsetVariable<-sample(VariableIndex,Q)
     subsetSample<-sample(SampleIndex,Qs)
     
     Xcal<-X[subsetSample,subsetVariable]
     ycal<-y[subsetSample]
     
     subsetSample_test<-setdiff(SampleIndex,subsetSample)
     Xtest<-X[subsetSample_test,subsetVariable]
     ytest<-y[subsetSample_test]
            

     #+++ Model selection by cross validation
     CV<-plsldacv(Xcal,ycal,nLV,fold)
     
     nLVr<-CV$optLV
     optLV[iter]<-nLVr

     #+++ PLS modeling
     PLSLDA<-plslda(Xcal,ycal,nLVr)

     #+++ Make predictions on the normal test set
     PRED<-plsldaval(PLSLDA,Xtest)
     error0[iter]<-sum(ytest!=PRED$YpredLabel)/Qv
      
     #+++ Make predictions on the permuted test set
     for (i in 1:Q)
     {
       columni<-Xtest[,i]
       Xp<-Xtest
       perm<-sample(Qv)
       Xp[,i]<-columni[perm]
       PRED<-plsldaval(PLSLDA,Xp)          
       error1[iter,subsetVariable[i]]<-sum(ytest!=PRED$YpredLabel)/Qv
     }      

     if (iter%%100 == 0){
	cat("Iteration = ", iter, "\n")
     }	
}

#+++ Computing p-value for each variable

for (i in 1:Num_col){

  K<-which(!is.na(error1[,i])==TRUE) #+++ find the models containing the ith variable.
  npe<-error0[K]+rnorm(length(K))/100000000     #add a very small number.
  ppe<-error1[K,i]+rnorm(length(K))/100000000   #add a very small number.
  
  #  Wilcox-Mann-Whitney U test.
  test_result<-wilcox.test(x=npe,y=ppe,alternative ="two.sided",mu = 0,paired = FALSE)
  p[i]<-test_result$p.value
  
  # detect the interfering variables.
  if (mean(npe)>mean(ppe)){p[i]<-1+p[i]
  interfer[i]<-1}     
  
}

# Transform p-value into COSS value
COSS<--log10(p)

#+++ Ranking the variables
rankvar<-order(COSS,decreasing=TRUE)

SPA<-list(optLV=optLV,error0=error0,error1=error1,interfer=interfer,p=p,COSS=COSS,RankedVariable=rankvar)
return(SPA)  #+++ Returning list data from SPA program

}         #+++ END for function


 #+++ END END END END END END. Put your heart into it.
 #+++ I know where I am.
 #+++ Nature with rain.



