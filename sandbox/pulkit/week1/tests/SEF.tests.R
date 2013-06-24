library(RUnit)
library(PerformanceAnalytics)
data(edhec)

test_PSR<-function(){ 
 checkEqualsNumeric(ProbSharpeRatio(edhec[,1],refSR = 0.28),0.6275225,tolerance = 1.0e-6)
}

test_MinTRL<-function(){  
  checkEqualsNumeric(MinTrackRecord(edhec[,1],refSR=0.28),3861.706,tolerance = 1.0e-3)
}
