#' M squared for Sortino of the return distribution
#'
#' M squared for Sortino is a M^2 calculated for Downside risk instead of Total Risk
#'
#' \deqn{M^2_S = r_P + Sortino ratio * (\sigma_{DM} - \sigma_D)}{M^2 (Sortino) = Rp + Sortino ratio * (DownsideRiskBenchmark - DownsideRiskPortfolio)}
#'
#' where \eqn{M^2_S} is MSquared for Sortino, \eqn{r_P} is the annualised portfolio return,
#' \eqn{\sigma_{DM}} is the benchmark annualised downside risk and \eqn{D} is the portfolio
#' annualised downside risk
#'
#' @aliases M2Sortino
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset return
#' @param Rb return vector of the benchmark asset 
#' @param MAR the minimum acceptable return
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.102-103
#' 
###keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' MAR = 0.005
#' print(M2Sortino(portfolio_bacon[,1], portfolio_bacon[,2], MAR)) #expected 0.1035
#'
#' data(managers)
#' MAR = 0
#' print(MSquaredExcess(managers['1996',1], managers['1996',8], MAR))
#' print(MSquaredExcess(managers['1996',1:5], managers['1996',8], MAR))
#'
#' @export 
M2Sortino <-
function (Ra, Rb, MAR = 0, ...)
{
    Ra = checkData(Ra)
    Rb = checkData(Rb)

    if (ncol(Ra)==1 || is.null(Ra) || is.vector(Ra)) {
    calcul = FALSE    
     for (i in (1:length(Ra))) {
     	 if (!is.na(Ra[i])) {
     	    calcul = TRUE
	 }
      }

     if (calcul) {
     	Period = Frequency(Rb)
        Rp = (prod(1 + Ra))^(Period / length(Ra)) - 1
	SigmaD = DownsideDeviation(Ra,MAR)*sqrt(Period)
	SigmaDM = DownsideDeviation(Rb,MAR)*sqrt(Period)
	SR = SortinoRatio(Ra,MAR)

	result = Rp + SR * (SigmaDM - SigmaD)
     }    
     else {
        result = NA
     }
      return(result)
    }
    else {
        result = apply(Ra, MARGIN = 2, M2Sortino, Rb = Rb, MAR = MAR, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("M2Sortino (MAR = ",MAR,")", sep="")
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
