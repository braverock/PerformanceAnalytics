#' @title  Table of Unsmooth Returns
#' 
#' @description Creates a table of estimates of moving averages for comparison across
#' multiple instruments or funds as well as their standard error and
#' smoothing index , which is Compenent Decomposition of Table of Unsmooth Returns
#' 
#' @details The estimation method is based on a maximum likelihood estimation of a moving average 
#' process (we use the innovations algorithm proposed by \bold{Brockwell and Davis} [1991]). The first 
#' step of this approach consists in computing a series of de-meaned observed returns:
#' \deqn{X(t) = R(0,t)- \mu}
#' where \eqn{\mu} is the expected value of the series of observed returns.
#' As a consequence, the above equation can be written as :
#' \deqn{X(t)= \theta(0)\eta(t) + \theta(1)\eta(t-1) + .....   + \theta(k)\eta(t-k)}
#' with the additional assumption that : \bold{\eqn{\eta(k)= N(0,\sigma(\eta)^2)}}
#' The structure of the model and the two constraints suppose that the complete integration of 
#'information in the price of the considered asset may take up to k periods because of its illiquidity. 
#'In addition, according to Getmansky et al., this model is in line with previous models of nonsynchronous trading such as the one developed by \bold{Cohen, Maier, Schwartz and Whitcomb} 
#' [1986].
#' Smoothing has an impact on the third and fourth moments of the returns distribution too.
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param n number of series lags
#' @param p confidence level for calculation, default p=.99
#' @param digits number of digits to round results to
#' @references   Cavenaile, Laurent, Coen, Alain and Hubner, Georges,\emph{ The Impact of Illiquidity and Higher Moments of Hedge Fund Returns on Their Risk-Adjusted Performance and Diversification Potential} (October 30, 2009). Journal of Alternative Investments, Forthcoming. Available at SSRN: \url{http://ssrn.com/abstract=1502698} Working paper is at \url{http://www.hec.ulg.ac.be/sites/default/files/workingpapers/WP_HECULg_20091001_Cavenaile_Coen_Hubner.pdf}
#' @author Peter Carl, Brian Peterson, Shubhankit Mohan
#' @keywords ts smooth return models
#' @seealso Reutrn.Geltner Reutrn.GLM  Return.Okunev
#' 
#' 
#' @rdname table.UnsmoothReturn
#' @examples
#' library(PerformanceAnalytics)
#' library(tseries)
#' data(managers)
#' table.UnsmoothReturn(managers,3)
#' @export 
table.UnsmoothReturn <-
  function (R, n = 2, p= 0.95, digits = 4)
  {# @author 
    
    # DESCRIPTION:
    # Downside Risk Summary: Statistics and Stylized Facts
    
    # Inputs:
    # R: a regular timeseries of returns (rather than prices)
    # n : Number of lags
    # p = Confifence Level
    # Output:
    # A table of estimates of Moving Average
    
    y = checkData(R, method = "xts")
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)
    
    # for each column, do the following:
    for(column in 1:columns) {
      x = y[,column]
    x=na.omit(x)
      ma.stats= arma(x, order = c(0, 2))

      z = c(as.numeric(ma.stats$coef[1]),
        sqrt(as.numeric(ma.stats$vcov[1]))*100,
            as.numeric(ma.stats$coef[2]),
            sqrt(as.numeric(ma.stats$vcov[4]))*100,sum(as.numeric(ma.stats$coef[1:2])*as.numeric(ma.stats$coef[1:2])))
      znames = c(
        "MA(1)",
        "Std Error of MA(1)(in %)",
        "MA(2)",
        "Std Error of MA(2)(in %)",
        "Smoothing Invest"
        
      )
      if(column == 1) {
        resultingtable = data.frame(Value = z, row.names = znames)
      }
      else {
        nextcolumn = data.frame(Value = z, row.names = znames)
        resultingtable = cbind(resultingtable, nextcolumn)
      }
    }
    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans

    
}

###############################################################################
# R (http://r-project.org/) 
#
# Copyright (c) 2004-2013 
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.UnSmoothReturn.R 
#
###############################################################################
