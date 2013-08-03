#' True returns represent the flow of information that would determine the equilibrium
#' value of the fund's securities in a frictionless market. However, true economic
#' returns are not observed. Instead, Rot
#' denotes the reported or observed return in
#' period t, which is a weighted average of the fund's true returns over the most recent k þ 1
#' periods, includingthe current period.
#' This averaging process captures the essence of smoothed returns in several
#' respects. From the perspective of illiquidity-driven smoothing, is consistent
#' with several models in the nonsynchronous tradingliterat ure. For example, Cohen
#' et al. (1 986, Chapter 6.1) propose a similar weighted-average model for observed
#' returns.
#' 
#' The Geltner autocorrelation adjusted return series may be calculated via:
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns

#' @param q order of autocorrelation coefficient
#' @author R
#' @references "An econometric model of serial correlation and
#' illiquidity in hedge fund returns
#' Mila Getmansky1, Andrew W. Lo*, Igor Makarov
#' MIT Sloan School of Management, 50 Memorial Drive, E52-432, Cambridge, MA 02142-1347, USA
#' Received 16 October 2002; received in revised form 7 March 2003; accepted 15 May 2003
#' Available online 10 July 2004
#' 
#'
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' Return.GLM(edhec,4)
#' 
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
      ma.coeff = as.numeric((arma(edhec[,1],order=c(0,q)))$coef[1:q])
 column.glm = ma.coeff[q]*lag(column.R,q)
     
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
# $Id: Return.GLM.R 2163 2012-07-16 00:30:19Z braverock $
#
###############################################################################
