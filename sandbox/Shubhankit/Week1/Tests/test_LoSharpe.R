library(RUnit)
library(PerformanceAnalytics)
data(edhec)

test_LoSharpe<-function(){
  
  checkEqualsNumeric(LoSharpe(edhec,0,3)[1],0.1366338,tolerance = 1.0e-6)
  
}