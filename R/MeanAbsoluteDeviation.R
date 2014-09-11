#' Mean absolute deviation of the return distribution
#'
#' To calculate Mean absolute deviation we take the sum of the absolute value of the difference between the returns and the mean of the returns and we divide it by the number of returns.
#'
#' \deqn{MeanAbsoluteDeviation = \frac{\sum^{n}_{i=1}\mid r_i - \overline{r}\mid}{n}}{MeanAbsoluteDeviation = sum(|r-mean(r)|)/n }
#'
#' where \eqn{n} is the number of observations of the entire series, \eqn{r_i} is the
#' return in month i and \eqn{\overline{r}} is the mean return
#' 
#' @aliases MeanAbsoluteDeviation
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.62
#' 
###keywords ts multivariate distribution models
#' @examples
#' data(portfolio_bacon)
#' print(MeanAbsoluteDeviation(portfolio_bacon[,1])) #expected 0.0310
#'
#' data(managers)
#' print(MeanAbsoluteDeviation(managers['1996']))
#' print(MeanAbsoluteDeviation(managers['1996',1]))
#'
#' @export 

MeanAbsoluteDeviation <- function (R, ...)
{
    R = checkData(R)

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       R = na.omit(R)
       result = sum(abs(R - mean(R)))/length(R)
       return(result)
    }  
    else {
        result = apply(R, MARGIN = 2, MeanAbsoluteDeviation, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("Mean absolute deviation", sep="")
        return(result)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
