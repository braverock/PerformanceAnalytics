#' InformationRatio = ActivePremium/TrackingError
#' 
#' The Active Premium divided by the Tracking Error.
#' 
#' InformationRatio = ActivePremium/TrackingError
#' 
#' This relates the degree to which an investment has beaten the benchmark to
#' the consistency with which the investment has beaten the benchmark.
#' 
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @note William Sharpe now recommends \code{InformationRatio} preferentially
#' to the original \code{\link{SharpeRatio}}.
#' @author Peter Carl
#' @seealso \code{\link{TrackingError}} \cr \code{\link{ActivePremium}} \cr
#' \code{\link{SharpeRatio}}
#' @references Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio
#' Management},Fall 1994, 49-58.
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' InformationRatio(managers[,"HAM1",drop=FALSE], managers[, "SP500 TR", drop=FALSE])
#' InformationRatio(managers[,1:6], managers[,8,drop=FALSE])
#' InformationRatio(managers[,1:6], managers[,8:7])
#' 
#' @export
InformationRatio <-
function (Ra, Rb, scale = NA)
{ # @author Peter Carl

    # DESCRIPTION
    # InformationRatio = ActivePremium/TrackingError

    # FUNCTION
    Ra = checkData(Ra)
    Rb = checkData(Rb)

    Ra.ncols = NCOL(Ra) 
    Rb.ncols = NCOL(Rb)

    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)

    if(is.na(scale)) {
        freq = periodicity(Ra)
        switch(freq$scale,
            minute = {stop("Data periodicity too high")},
            hourly = {stop("Data periodicity too high")},
            daily = {scale = 252},
            weekly = {scale = 52},
            monthly = {scale = 12},
            quarterly = {scale = 4},
            yearly = {scale = 1}
        )
    }

    ir <-function (Ra, Rb, scale)
    {
        ap = ActivePremium(Ra, Rb, scale = scale)
        te = TrackingError(Ra, Rb, scale = scale)
        IR = ap/te
        return(IR)
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb, scale) ir(Ra[,n[1]], Rb[,n[2]], scale), Ra = Ra, Rb = Rb, scale = scale)

    if(length(result) ==1)
        return(result)
    else {
        result = matrix(result, ncol=Ra.ncols, nrow=Rb.ncols, byrow=TRUE)
        rownames(result) = paste("Information Ratio:", colnames(Rb))
        colnames(result) = colnames(Ra)
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
