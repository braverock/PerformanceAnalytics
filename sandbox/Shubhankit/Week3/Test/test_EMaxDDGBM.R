library(RUnit)
library(PerformanceAnalytics)
data(edhec[,1])

test_EMaxDDGBM<-function(){
  
  checkEqualsNumeric(EMaxDDGBM(edhec[,1])[1],1.708261,tolerance = 1.0e-6)
  
}