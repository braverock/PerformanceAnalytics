#' Fama beta of the return distribution
#'
#' Fama beta is a beta used to calculate the loss of diversification. It is made
#' so that the systematic risk is equivalent to the total portfolio risk.
#'
#' \deqn{\beta_F = \frac{\sigma_P}{\sigma_M}}{Fama beta = portfolio standard deviation / benchmark standard deviation}
#'
#' where \eqn{\sigma_P} is the portfolio standard deviation and \eqn{\sigma_M} is the
#' market risk
#'
#' @aliases FamaBeta
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset 
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.78
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(FamaBeta(portfolio_bacon[,1], portfolio_bacon[,2])) #expected 1.03
#'
#' data(managers)
#' print(FamaBeta(managers['1996',1], managers['1996',8]))
#' print(FamaBeta(managers['1996',1:5], managers['1996',8]))
#'
#' @export 
FamaBeta <-
function (Ra, Rb, ...)
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
     	n1 = length(Ra)
	n2 = length(Rb)
        Period1 = Frequency(Ra)
	Period2 = Frequency(Rb)
        result = sqrt(var(Ra)*(n1-1)/(n1))*sqrt(Period1) / (sqrt(var(Rb)*(n2-1)/n2)*sqrt(Period2))
     }    
     else {
        result = NA
     }
      return(result)
    }
    else {
        Ra = checkData(Ra)
        result = apply(Ra, MARGIN = 2, FamaBeta, Rb = Rb, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("Fama Beta ", sep="")
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
