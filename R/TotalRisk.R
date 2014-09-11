#' Total risk of the return distribution
#'
#' The square of total risk is the sum of the square of systematic risk and the square
#' of specific risk. Specific risk is the standard deviation of the error term in the
#' regression equation. Both terms are annualized to calculate total risk.
#'
#' \deqn{Total Risk = \sqrt{Systematic Risk^2 + Specific Risk^2}}{Total Risk^2 = Systematic Risk^2 + Specific Risk^2}
#'
#' @aliases TotalRisk
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.75
#' 
###keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(TotalRisk(portfolio_bacon[,1], portfolio_bacon[,2])) #expected 0.0134
#'
#' data(managers)
#' print(TotalRisk(managers['1996',1], managers['1996',8]))
#' print(TotalRisk(managers['1996',1:5], managers['1996',8]))
#'
#' @export 
TotalRisk <-
function (Ra, Rb, Rf = 0,  ...)
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
     	epsilon = Ra - Rb * CAPM.beta(Ra,Rb,Rf) - CAPM.alpha(Ra,Rb,Rf)
	specifikRisk = sqrt(sum((epsilon - mean(epsilon))^2)/length(epsilon))*sqrt(Period)
        result = sqrt((SystematicRisk(Ra,Rb,Rf))^2 + specifikRisk^2)
     }    
     else {
        result = NA
     }
      return(result)
    }
    else {
        Ra = checkData(Ra)
        result = apply(Ra, MARGIN = 2, TotalRisk, Rb = Rb, Rf = Rf, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("Total Risk = ", sep="")
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
