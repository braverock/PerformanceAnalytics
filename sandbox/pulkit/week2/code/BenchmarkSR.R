#'@title 
#'Benchmark Sharpe Ratio
#'
#'@description
#'The benchmark SR is a linear function of the average
#' SR of the individual strategies, and a decreasing
#' convex function of the number of strategies and the 
#' average pairwise correlation. The Returns are given as
#' the input with the benchmark Sharpe Ratio as the output.
#' 
#'@aliases BenchmarkSR
#'
#'@param R a vector, matrix, data frame,timeseries or zoo object of asset returns
#'
#'@references
#'
#'@export
BenchmanrkSR<-function(R){
  
  x = checkData(R)
  columns = ncol(x)
  SR = SharpeRatio(x)
  sr_avg = mean(SR)
  corr = table.Correlation(edhec,edhec)
  corr_avg = 0
  for(i in 1:columns){
    for(j in i:columns){
      corr_avg = corr_avg + corr[(i-1)*columns+j,]
    }
  }
  SR_Benchmark = sr_avg*sqrt(columns/(1+(columns-1))*corr_avg[1,1])
  return(SR_Benchmark)
}