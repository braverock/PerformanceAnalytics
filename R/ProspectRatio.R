#' Prospect ratio of the return distribution
#'
#' Prospect ratio is a ratio used to penalise loss since most people feel loss
#' greater than gain
#'
#' \deqn{ProspectRatio(R) = \frac{\frac{1}{n}*\sum^{n}_{i=1}(Max(r_i,0)+2.25*Min(r_i,0) - MAR)}{\sigma_D}}{ProspectRatio(R) = (1/n * sum(Max(ri,0) + 2.25 * Min(ri,0)) - MAR) / DownsideRisk}
#'
#' where \eqn{n} is the number of observations of the entire series, MAR is the minimum acceptable return and \eqn{\sigma_D} is the downside risk
#' 
#' @aliases ProspectRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param MAR the minimum acceptable return
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.100
#' 
###keywords ts multivariate distribution models
#' @examples
#' data(portfolio_bacon)
#' MAR = 0.05
#' print(ProspectRatio(portfolio_bacon[,1], MAR)) #expected -0.134
#'
#' data(managers)
#' MAR = 0
#' print(ProspectRatio(managers['1996'], MAR))
#' print(ProspectRatio(managers['1996',1], MAR))
#'
#' @export

ProspectRatio <- function (R, MAR, ...)
{
    R <- checkData(R)
    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       calcul = FALSE    
       for (i in (1:length(R))) {
     	 if (!is.na(R[i])) {
     	    calcul = TRUE
	    }
      }

       if (calcul) {
	  n = length(R)
	  SigD = DownsideDeviation(R,MAR)
          R = na.omit(R)
          r1 = R[which(R > 0)]
          r2 = R[which(R < 0)]
	  result = (sum(r1)+2.25*sum(r2)-MAR)/(SigD*n)
	}
	else
	{
	 result = NA
	}
       return(result)
    }  
    else {
        result = apply(R, MARGIN = 2, ProspectRatio, MAR=MAR, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("Prospect ratio (MAR = ",MAR,"%)", sep="")
        return(result)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
