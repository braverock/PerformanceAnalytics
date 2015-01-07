#' M squared excess of the return distribution
#'
#' M squared excess is the quantity above the standard M. There is a geometric excess return which is better for Bacon and an arithmetic excess return
#'
#' \deqn{M^2 excess (geometric) = \frac{1 + M^2}{1 + b} - 1}{MSquared excess (geometric) = (1+M^2)/(1+b) - 1}
#' \deqn{M^2 excess (arithmetic) = M^2 - b}{MSquared excess (arithmetic) = M^2 - b}
#'
#' where \eqn{M^2} is MSquared and \eqn{b} is the benchmark annualised return.
#'
#' @aliases MSquaredExcess
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset return
#' @param Rb return vector of the benchmark asset 
#' @param Rf risk free rate, in same period as your returns
#' @param Method one of "geometric" or "arithmetic" indicating the method to use
#' to calculate MSquareExcess
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.68
#' 
###keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' MSquaredExcess(portfolio_bacon[,1], portfolio_bacon[,2]) #expected -0.00998
#' 
#' MSquaredExcess(portfolio_bacon[,1], portfolio_bacon[,2], Method="arithmetic") #expected -0.011
#'
#' data(managers)
#' MSquaredExcess(managers['1996',1], managers['1996',8])
#' MSquaredExcess(managers['1996',1:5], managers['1996',8])
#'
#' @export 
MSquaredExcess <-
function (Ra, Rb, Rf = 0, Method = c("geometric", "arithmetic"), ...)
{
    Method = Method[1]

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
     	Period = Frequency(Ra)
        Rbp = (prod(1 + Rb))^(Period / length(Rb)) - 1

        switch(Method,
            geometric = {result = (1+MSquared(Ra,Rb,Rf))/(1+Rbp) - 1},
            arithmetic = {result = MSquared(Ra,Rb,Rf) - Rbp} 
        ) # end switch
     }    
     else {
        result = NA
     }
      return(result)
    }
    else {
        result = apply(Ra, MARGIN = 2, MSquaredExcess, Rb = Rb, Rf = Rf, Period = Period, Method = Method, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("MSquaredExcess (Risk free = ",Rf,")", sep="")
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
