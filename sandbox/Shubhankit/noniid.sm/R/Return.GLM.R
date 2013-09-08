#' GLM Return Model
#'
#' True returns represent the flow of information that would determine the equilibrium
#' value of the fund's securities in a frictionless market. However, true economic
#' returns are not observed. The returns to hedge funds and other alternative investments are often 
#' highly serially correlated.We propose an econometric model of return smoothing and \emph{develop estimators for the smoothing 
#' profile as well as a smoothing-adjusted Sharpe ratio}.
#' @examples
#' data(edhec)
#' Return.GLM(edhec,4)
#' @param 
#' Ra : an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param 
#' q : order of autocorrelation coefficient lag factors
#'  
#' @details
#' To quantify the impact of all of these possible sources of serial correlation, denote by R(t)
#' the true economic return of a hedge fund in period 't'; and let R(t) satisfy the following linear 
#' single-factor model: where:  
#' \deqn{R(0,t) = \theta_{0}R(t) + \theta_{1}R(t-1) + \theta_{2}R(t-2) ....  + \theta_{k}R(t-k)} 
#' Where : \eqn{\theta}'i is defined as the weighted lag of autocorrelated lag and whose sum is 1.
#' \deqn{\theta (j) \epsilon [0,1] where : j = 0,1,....,k  }
#' and,
#' \deqn{\theta _1 + \theta _2 + \theta _3 \cdots + \theta _k = 1}
#'Using the methods outlined above , the paper estimates the smoothing model
#' using maximumlikelihood procedure-programmed in Matlab using the Optimization Toolbox andreplicated in Stata usingits MA(k) estimation routine.Using Time seseries analysis and computational finance("\bold{tseries}") library , we fit an it an \bold{ARMA} model to a univariate time series by conditional least squares. For exact maximum likelihood estimation,arima0 from package \bold{stats} can be used.
#'
#' @author Brian Peterson,Peter Carl, Shubhankit Mohan
#' @references Mila Getmansky, Andrew W. Lo, Igor Makarov,\emph{An econometric model of serial correlation and 
#' and illiquidity in hedge fund Returns},Journal of Financial Economics 74 (2004).\url{ http://ssrn.com/abstract=384700}
#' @keywords ts multivariate distribution model
#' @seealso Return.Geltner
#' @export
Return.GLM <-
  function (Ra,q=3)
  { # @author Brian G. Peterson, Peter Carl
    
    # Description:
   
    # Ra    return vector
    # q     Lag Factors
    # Function:
    library(tseries)
    library(PerformanceAnalytics)
    R = checkData(Ra, method="xts")
    # Get dimensions and labels
    columns.a = ncol(R)
    columnnames.a = colnames(R)
    
    clean.GLM <- function(column.R,q=3) {
      ma.coeff = as.numeric((arma(column.R,order=c(0,q)))$coef[1:q])
 column.glm = (1-ma.coeff[q])*lag(column.R,q)
     
    return(column.glm)
    }
    
    for(column.a in 1:columns.a) { # for each asset passed in as R
      # clean the data and get rid of NAs
      column.glma = na.skip(R[,column.a],clean.GLM)
      
      if(column.a == 1)  { glm = column.glma }
      else { glm = cbind (glm, column.glma) }
      
    }
    
    colnames(glm) = columnnames.a
    
    # RESULTS:
    return(reclass(glm,match.to=Ra))
    
  }

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.GLM.R 2334 2013-04-01 16:57:25Z braverock $
#
###############################################################################
