sesp<-function(score,class){

#+++ Compute the sensitivity and specificity for binary classification.
# class has to be 1 or -1


class<-sign(class)
score<-sign(score)
subsetp<-which(class==1)
subsetn<-which(class==-1)

# calculation
accuracy<-sum(class==score)/length(class)
sensitivity<-sum(class[subsetp]==score[subsetp])/length(subsetp)
specificity<-sum(class[subsetn]==score[subsetn])/length(subsetn)


Result<-list(accuracy=accuracy,sensitivity=sensitivity,specificity=specificity)

return(Result)

}


#+++ There is a song you like to sing.

#+++ Nature with rain.