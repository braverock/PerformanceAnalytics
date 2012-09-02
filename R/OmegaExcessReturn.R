#' Omega excess return of the return distribution
#'
#' Omega excess return is another form of downside risk-adjusted return. It is
#' calculated by multiplying the downside variance of the style benchmark by 3
#' times the style beta.
#'
#' \deqn{\omega = r_P - 3*\beta_S*\sigma_{MD}^2}{ OmegaExcessReturn = Portfolio return - 3*style beta*style benchmark variance squared}
#'
#' where \eqn{\omega} is omega excess return, \eqn{\beta_S} is style beta, \eqn{\sigma_D} 
#' is the portfolio annualised downside risk and \eqn{\sigma_{MD}} is the benchmark annualised downside risk.
#'
#' @aliases OmegaExessReturn
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param MAR the minimum acceptable return
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.103
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' MAR = 0.005
#' print(OmegaExcessReturn(portfolio_bacon[,1], portfolio_bacon[,2], MAR)) #expected 0.0805
#'
#' data(managers)
#' MAR = 0
#' print(OmegaExcessReturn(managers['1996',1], managers['1996',8], MAR))
#' print(OmegaExcessReturn(managers['1996',1:5], managers['1996',8], MAR))
#'
#' @export 

OmegaExcessReturn <-
function (Ra, Rb, MAR = 0, ...)
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
	Rp = (prod(1 + Ra))^(Period / length(Ra)) - 1
	SigmaD = DownsideDeviation(Ra,MAR)*sqrt(Period)
	SigmaDM = DownsideDeviation(Rb,MAR)*sqrt(Period)
        result = Rp - 3 * SigmaD * SigmaDM 
     }    
     else {
        result = NA
     }
      return(result)
    }
    else {
        Ra = checkData(Ra)
        result = apply(Ra, MARGIN = 2, OmegaExcessReturn, Rb = Rb, MAR = MAR, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("Omega Excess Return (MAR = ",MAR,")", sep="")
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
