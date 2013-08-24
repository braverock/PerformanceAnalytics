#'@title Andrew Lo Sharpe Ratio
#'@description
#' Although the Sharpe ratio has become part of the canon of modern financial 
#' analysis, its applications typically do not account for the fact that it is an
#' estimated quantity, subject to estimation errors that can be substantial in 
#' some cases.
#' 
#' Many studies have documented various violations of the assumption of 
#' IID returns for financial securities.
#' 
#' Under the assumption of stationarity,a version of the Central Limit Theorem can 
#' still be  applied to the estimator .
#' @details
#' The relationship between SR and SR(q) is somewhat more involved for non-
#'IID returns because the variance of Rt(q) is not just the sum of the variances of component returns but also includes all the covariances. Specifically, under
#' the assumption that returns \eqn{R_t}  are stationary,
#' \deqn{ Var[(R_t)] =   \sum \sum Cov(R(t-i),R(t-j)) = q{\sigma^2} + 2{\sigma^2} \sum (q-k)\rho(k) }
#' Where  \eqn{ \rho(k) = Cov(R(t),R(t-k))/Var[(R_t)]} is the \eqn{k^{th}} order autocorrelation coefficient of the series of returns.This yields the following relationship between SR and SR(q):
#' and i,j belongs to 0 to q-1
#'\deqn{SR(q)  =  \eta(q) }
#'Where :
#' \deqn{ }{\eta(q) = [q]/[\sqrt(q\sigma^2) + 2\sigma^2 \sum(q-k)\rho(k)] }
#' Where k belongs to 0 to q-1
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' daily asset returns
#' @param Rf an xts, vector, matrix, data frame, timeSeries or zoo object of
#' annualized Risk Free Rate
#' @param q Number of autocorrelated lag periods. Taken as 3 (Default)
#' @param \dots any other pass thru parameters
#' @author Brian G. Peterson, Peter Carl, Shubhankit Mohan
#' @references Getmansky, Mila, Lo, Andrew W. and Makarov, Igor,\emph{ An Econometric Model of Serial Correlation and Illiquidity in Hedge Fund Returns} (March 1, 2003). MIT Sloan Working Paper No. 4288-03; MIT Laboratory for Financial Engineering Working Paper No. LFE-1041A-03; EFMA 2003 Helsinki Meetings.
#'\code{\link[stats]{}} \cr
#' \url{http://ssrn.com/abstract=384700}
#' @keywords ts multivariate distribution models non-iid 
#' @examples
#' 
#' data(edhec)
#' head(LoSharpe(edhec,0,3)
#' @rdname LoSharpe
#' @export
LoSharpe <-
  function (Ra,Rf = 0,q = 3, ...)
  { # @author Brian G. Peterson, Peter Carl
    
    
    # Function:
    R = checkData(Ra, method="xts")
    # Get dimensions and labels
    columns.a = ncol(R)
    columnnames.a = colnames(R)
    # Time used for daily Return manipulations
    Time= 252*nyears(edhec)
    clean.lo <- function(column.R,q) {
      # compute the lagged return series
      gamma.k =matrix(0,q)
      mu = sum(column.R)/(Time)
      Rf= Rf/(Time)
      for(i in 1:q){
        lagR = lag(column.R, k=i)
        # compute the Momentum Lagged Values
        gamma.k[i]= (sum(((column.R-mu)*(lagR-mu)),na.rm=TRUE))
      }
      return(gamma.k)
    }
    neta.lo <- function(pho.k,q) {
      # compute the lagged return series
      sumq = 0
      for(j in 1:q){
        sumq = sumq+ (q-j)*pho.k[j]
      }
      return(q/(sqrt(q+2*sumq)))
    }
    for(column.a in 1:columns.a) { # for each asset passed in as R
      # clean the data and get rid of NAs
      mu = sum(R[,column.a])/(Time)
      sig=sqrt(((R[,column.a]-mu)^2/(Time)))
      pho.k = clean.lo(R[,column.a],q)/(as.numeric(sig[1]))
      netaq=neta.lo(pho.k,q)
      column.lo = (netaq*((mu-Rf)/as.numeric(sig[1])))
      
      if(column.a == 1)  { lo = column.lo }
      else { lo = cbind (lo, column.lo) }
      
    }
    colnames(lo) = columnnames.a
    rownames(lo)= paste("Lo Sharpe Ratio")
    return(lo)
    
    
    # RESULTS:
    
  }
