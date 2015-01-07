#' Calculate Tracking Error of returns against a benchmark
#' 
#' A measure of the unexplained portion of performance relative to a benchmark.
#' 
#' Tracking error is calculated by taking the square root of the average of the
#' squared deviations between the investment's returns and the benchmark's
#' returns, then multiplying the result by the square root of the scale of the
#' returns.
#' 
#' \deqn{ TrackingError =
#' \sqrt{\sum\frac{(R_{a}-R_{b})^{2}}{len(R_{a})\sqrt{scale}}} }{ TrackingError
#' = sqrt(sum(Ra - Rb)^2 / (length(R) - 1)) * sqrt(scale)}
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @author Peter Carl
#' @seealso \code{\link{InformationRatio}} \code{\link{TrackingError}}
#' @references Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio
#' Management},Fall 1994, 49-58.
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' TrackingError(managers[,1,drop=FALSE], managers[,8,drop=FALSE]) 
#' TrackingError(managers[,1:6], managers[,8,drop=FALSE]) 
#' TrackingError(managers[,1:6], managers[,8:7,drop=FALSE])
#' 
#' @export
TrackingError <-
function (Ra, Rb, scale = NA)
{ # @author Peter Carl

    # DESCRIPTION
    # TrackingError = sqrt(sum(assetReturns.vec - benchmarkReturns.vec)^2 /
    #                   (length(assetReturns.vec) - 1)) * sqrt(scale)

    # Inputs:
    # Outputs:

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

    te <-function (Ra, Rb, scale)
    {
        TE = sd.xts(Return.excess(Ra, Rb), na.rm=TRUE) * sqrt(scale)
        return(TE)
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb, scale) te(Ra[,n[1]], Rb[,n[2]], scale), Ra = Ra, Rb = Rb, scale = scale)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Tracking Error:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
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
