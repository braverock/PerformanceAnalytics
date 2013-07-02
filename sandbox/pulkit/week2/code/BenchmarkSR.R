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
#'\deqn{SR_B = \bar{SR}\sqrt{\frac{S}{1+(S-1)\bar{\rho}}}}
#'
#'@param R a vector, matrix, data frame,timeseries or zoo object of asset returns
#'
#'@references
#'Bailey, David H. and Lopez de Prado, Marcos, The Strategy Approval Decision: 
#'A Sharpe Ratio Indifference Curve Approach (January 2013). Algorithmic Finance, 
#'Vol. 2, No. 1 (2013).
#'
#'@examples
#'
#'data(edhec)
#'BenchmarkSR(edhec) #expected 0.2019308
#'
#'@export
#'
BenchmarkSR<-function(R){
  x = checkData(R)
  columns = ncol(x)
  #TODO : What to do if the number of columns is only one ?  
  if(columns == 1){
    stop("The number of return series should be greater than 1")
  }
  SR = SharpeRatio(x)
  sr_avg = mean(SR)
  corr = table.Correlation(edhec,edhec)
  corr_avg = 0
  for(i in 1:(columns-1)){
    for(j in (i+1):columns){
      corr_avg = corr_avg + corr[(i-1)*columns+j,]
    }
  }
  corr_avg = corr_avg*2/(columns*(columns-1))
  SR_Benchmark = sr_avg*sqrt(columns/(1+(columns-1)*corr_avg[1,1]))
  return(SR_Benchmark)
}