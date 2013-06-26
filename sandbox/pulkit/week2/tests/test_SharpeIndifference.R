library(RUnit)
library(PerformanceAnalytics)
data(edhec)

test_BenchmarkSR<-function(){
  
  checkEqualsNumeric(BenchmanrkSR(edhec),0.170288,tolerance = 1.0e-6)
  
}