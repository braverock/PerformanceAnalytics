mbb <- function(R, N=length(R), k=6, nrep=1000, verbose=TRUE, cores=6){
#' Moving blocks bootstrap 
#' 
#' Follows Efron and Tibshirani (sec. 8.6).
#' With a nod to http://stat.wharton.upenn.edu/~buja/STAT-541/time-series-bootstrap.R

#' N length of the resulting time series
#' k size of the moving blocks
#' nrep number of bootstrap replications
  require(foreach)
  require(doMC)
  registerDoMC(cores)
  .mdd <- function(R, N, k){
    result.mbb <- rep(NA, N) # local vector for a bootstrap replication
    for(j in 1:ceiling(N/k)) {  # fill the vector with random blocks
      endpoint <- sample(k:N, size=1) # by randomly sampling endpoints
      result.mbb[(j-1)*k+1:k] <- R[endpoint-(k:1)+1] # and copying blocks to the local vector
    }
    result <- as.matrix(result.mbb[1:N])# trim overflow when k doesn't divide N
    return(result)
  }
  
  result <- foreach(i=1:nrep, .combine=cbind, .inorder=TRUE) %dopar% {
    .mdd(R=R, N=N, k=k)
  }
  return(result)
}