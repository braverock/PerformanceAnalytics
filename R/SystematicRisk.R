#' Systematic risk of the return distribution
#'
#' Systematic risk as defined by Bacon(2008) is the product of beta by market 
#' risk. Be careful ! It's not the same definition as the one given by Michael
#' Jensen. Market risk is the standard deviation of the benchmark. The systematic
#' risk is annualized
#'
#' \deqn{\sigma_s = \beta * \sigma_m}{systematic risk = beta * market risk}
#'
#' where \eqn{\sigma_s} is the systematic risk, \eqn{\beta} is the regression beta,
#' and \eqn{\sigma_m} is the market risk
#'
#' @aliases SystematicRisk
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset 
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.75
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(SystematicRisk(portfolio_bacon[,1], portfolio_bacon[,2])) #expected 0.013
#'
#' data(managers)
#' print(SystematicRisk(managers['1996',1], managers['1996',8]))
#' print(SystematicRisk(managers['1996',1:5], managers['1996',8]))
#'
#' @export 
SystematicRisk <-
function (Ra, Rb, Rf = 0, ...)
{
    calcul = FALSE
    Ra = checkData(Ra, method="matrix")
    Rb = checkData(Rb, method="matrix")

    if (ncol(Ra)==1 || is.null(Ra) || is.vector(Ra)) {
    
     for (i in (1:length(Ra))) {
     	 if (!is.na(Ra[i])) {
     	    calcul = TRUE
	 }
      }

     if (calcul) {
     	Period = Frequency(Ra)
        result = CAPM.beta(Ra,Rb,Rf) * sqrt(sum((Rb-mean(Rb))^2)/length(Rb))*sqrt(Period)
     }    
     else {
        result = NA
     }
      return(result)
    }
    else {
        Ra = checkData(Ra)
        result = apply(Ra, MARGIN = 2, SystematicRisk, Rb = Rb, Rf = Rf, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("Systematic Risk (Risk free = ",Rf,")", sep="")
        return(result)
    }
}


###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
