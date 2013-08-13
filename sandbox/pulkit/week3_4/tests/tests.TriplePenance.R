library(RUnit)
library(PerformanceAnalytics)
data(edhec)

test_MaxDD<-function(){ 
 checkEqualsNumeric(MaxDD(edhec[,1],0.95,"normal"),6.618966,tolerance = 1.0e-6)
}

test_MinTRL<-function(){  
  checkEqualsNumeric(TuW(edhec[,1],0.95,"normal"),103.2573,tolerance = 1.0e-3)
}
