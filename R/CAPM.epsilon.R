#' Regression epsilon of the return distribution
#'
#' The regression epsilon is an error term measuring the vertical distance between
#' the return predicted by the equation and the real result.
#'
#' \deqn{\epsilon_r = r_p - \alpha_r - \beta_r * b}{epsilon_r = r_p - alpha_r - beta_r * b}
#'
#' where \eqn{\alpha_r} is the regression alpha, \eqn{\beta_r} is the regression beta,
#' \eqn{r_p} is the portfolio return and b is the benchmark return
#'
#' @aliases Regression epsilon
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.71
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(SFM.epsilon(portfolio_bacon[,1], portfolio_bacon[,2])) #expected -0.013
#'
#' data(managers)
#' print(SFM.epsilon(managers['1996',1], managers['1996',8]))
#' print(SFM.epsilon(managers['1996',1:5], managers['1996',8]))
#'
#' @rdname CAPM.epsilon
#' @export CAPM.epsilon SFM.epsilon
CAPM.epsilon <- SFM.epsilon <-
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
     Ra = na.omit(Ra)
     Rb = na.omit(Rb)

        if (calcul) {
	   period = Frequency(Ra)
           Rp = (prod(1 + Ra))^(period / length(Ra)) - 1
           Rpb = (prod(1 + Rb))^(period / length(Rb)) - 1
           result = Rf + Rp - CAPM.alpha(Ra,Rb,Rf) - (Rpb - Rf) * CAPM.beta(Ra,Rb,Rf) 
        }   
        else {
           result = NA
        }
        return(result)
    }
    else {
    	Ra = checkData(Ra)
        result = apply(Ra, MARGIN = 2, CAPM.epsilon, Rb = Rb, Rf = Rf, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("Regression epsilon (Risk free = ",Rf,")", sep="")
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
