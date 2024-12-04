#' Jensen's alpha of the return distribution
#'
#' The Jensen's alpha is the intercept of the regression equation in the Capital
#' Asset Pricing Model and is in effect the exess return adjusted for systematic risk.
#'
#' \deqn{\alpha = r_p - r_f - \beta_p * (b - r_f)}{alpha = r_p - r_f - beta_p * (b - r_f)}
#'
#' where \eqn{r_f} is the risk free rate, \eqn{\beta_r} is the regression beta,
#' \eqn{r_p} is the portfolio return and b is the benchmark return
#'
#' @aliases SFM.jensenAlpha
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other pass thru parameters
#' @param method (Optional): string representing linear regression model, "LS" for Least Squares
#'                    and "Rob" for robust      
#' @param family (Optional): 
#'         If method == "Rob": 
#'           This is a string specifying the name of the family of loss function
#'           to be used (current valid options are "bisquare", "opt" and "mopt").
#'           Incomplete entries will be matched to the current valid options. 
#'           Defaults to "mopt".
#'         Else: the parameter is ignored
#' @author Matthieu Lestel, Dhairya Jain
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.72
#' 
###keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(SFM.jensenAlpha(portfolio_bacon[,1], portfolio_bacon[,2])) #expected -0.014
#'
#' data(managers)
#' print(SFM.jensenAlpha(managers['1996',1], managers['1996',8]))
#' print(SFM.jensenAlpha(managers['1996',1:5], managers['1996',8]))
#'
#' @rdname CAPM.jensenAlpha
#' @export CAPM.jensenAlpha SFM.jensenAlpha

CAPM.jensenAlpha <- SFM.jensenAlpha <-
function (Ra, Rb, Rf = 0, ..., method="LS", family="mopt")
{
    calcul = FALSE
    Ra = checkData(Ra, method="matrix")
    Rb = checkData(Rb, method="matrix")

    if (ncol(Ra)==1 || is.null(Ra) || is.vector(Ra)) {
        period = Frequency(Ra)
        Rp = (prod(1 + Ra))^(period / length(Ra)) - 1
        Rpb = (prod(1 + Rb))^(period / length(Rb)) - 1
        for (i in (1:length(Ra))){
            if (!is.na(Ra[i])) {
                calcul = TRUE
            }
        } 
    
        if (calcul) {
            result = Rp - Rf - CAPM.beta(Ra,Rb,Rf,...,method=method,family=family) * (Rpb - Rf) 
        }    
        else {
            result = NA
        }
        return(result)
    }
    else {
        Ra = checkData(Ra)
        result = apply(Ra, MARGIN = 2, CAPM.jensenAlpha, Rb = Rb, Rf = Rf,
                       method = method, family = family, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("Jensen's Alpha (Risk free = ",Rf,")", sep="")
        return(result)
    }
}


###############################################################################
# R (https://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2020 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
