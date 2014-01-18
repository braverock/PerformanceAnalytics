#' Net selectivity of the return distribution
#'
#' Net selectivity is the remaining selectivity after deducting the amount of return
#' require to justify not being fully diversified
#'
#' If net selectivity is negative the portfolio manager has not justified the loss of
#' diversification
#'
#' \deqn{Net selectivity = \alpha - d}{Net selectivity = Selectity - diversification}
#'
#' where \eqn{\alpha} is the selectivity and \eqn{d} is the diversification
#'
#' @aliases NetSelectivity
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.78
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(NetSelectivity(portfolio_bacon[,1], portfolio_bacon[,2])) #expected -0.017
#'
#' data(managers)
#' print(NetSelectivity(managers['1996',1], managers['1996',8]))
#' print(NetSelectivity(managers['1996',1:5], managers['1996',8]))
#'
#' @export 

NetSelectivity <-
function (Ra, Rb, Rf = 0, ...)
{
    Ra = checkData(Ra, method="matrix")
    Rb = checkData(Rb, method="matrix")

    if (ncol(Ra)==1 || is.null(Ra) || is.vector(Ra)) {
    calcul = FALSE    
    for (i in (1:length(Ra))) {
     	 if (!is.na(Ra[i])) {
     	    calcul = TRUE
	 }
      }

     if (calcul) {
     	Period = Frequency(Ra)
     	b = (prod(1 + Rb))^(Period / length(Rb)) - 1
     	d = (FamaBeta(Ra,Rb)-CAPM.beta(Ra,Rb,Rf)) * (b - Rf)
        result = Selectivity(Ra,Rb,Rf) - d 
     }    
     else {
        result = NA
     }
      return(result)
    }
    else {
        Ra = checkData(Ra)
        result = apply(Ra, MARGIN = 2, NetSelectivity, Rb = Rb, Rf = Rf, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("Net Selectivity (Risk free = ",Rf,")", sep="")
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
