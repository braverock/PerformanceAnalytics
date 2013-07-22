library(RUnit)
library(PerformanceAnalytics)
data(edhec)

test_BenchmarkSR<-function(){
  
  checkEqualsNumeric(BenchmarkSR(edhec),0.393797,tolerance = 1.0e-6)
  
}