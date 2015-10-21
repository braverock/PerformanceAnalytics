#' calculate a Calmar or Sterling reward/risk ratio
#'  
#' Calmar and Sterling Ratios are yet another method of creating a
#' risk-adjusted measure for ranking investments similar to the
#' \code{\link{SharpeRatio}}.
#' 
#' Both the Calmar and the Sterling ratio are the ratio of annualized return
#' over the absolute value of the maximum drawdown of an investment. The
#' Sterling ratio adds an excess risk measure to the maximum drawdown,
#' traditionally and defaulting to 10\%.
#' 
#' It is also traditional to use a three year return series for these
#' calculations, although the functions included here make no effort to
#' determine the length of your series.  If you want to use a subset of your
#' series, you'll need to truncate or subset the input data to the desired
#' length.
#' 
#' Many other measures have been proposed to do similar reward to risk ranking.
#' It is the opinion of this author that newer measures such as Sortino's
#' \code{\link{UpsidePotentialRatio}} or Favre's modified
#' \code{\link{SharpeRatio}} are both \dQuote{better} measures, and
#' should be preferred to the Calmar or Sterling Ratio.
#' 
#' @aliases CalmarRatio SterlingRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param excess for Sterling Ratio, excess amount to add to the max drawdown,
#' traditionally and default .1 (10\%)
#' @author Brian G. Peterson
#' @seealso 
#' \code{\link{Return.annualized}}, \cr 
#' \code{\link{maxDrawdown}}, \cr
#' \code{\link{SharpeRatio.modified}}, \cr 
#' \code{\link{UpsidePotentialRatio}}
#' @references Bacon, Carl. \emph{Practical Portfolio Performance Measurement
#' and Attribution}. Wiley. 2004.
###keywords ts multivariate distribution models
#' @examples
#' 
#'     data(managers)
#'     CalmarRatio(managers[,1,drop=FALSE])
#'     CalmarRatio(managers[,1:6]) 
#'     SterlingRatio(managers[,1,drop=FALSE])
#'     SterlingRatio(managers[,1:6])
#' 
#' @export
#' @rdname CalmarRatio
CalmarRatio <- function (R, scale = NA)
{ # @author Brian G. Peterson

    # DESCRIPTION:
    # Inputs:
    # Ra: in this case, the function anticipates having a return stream as input,
    #    rather than prices.
    # scale: number of periods per year
    # Outputs:
    # This function returns a Calmar Ratio

    # FUNCTION:

    R = checkData(R)
    if(is.na(scale)) {
        freq = periodicity(R)
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
    annualized_return = Return.annualized(R, scale=scale)
    drawdown = abs(maxDrawdown(R))
    result = annualized_return/drawdown
    rownames(result) = "Calmar Ratio"
    return(result)
}

#' @export
#' @rdname CalmarRatio
SterlingRatio <-
function (R, scale=NA, excess=.1)
{ # @author Brian G. Peterson

    # DESCRIPTION:
    # Inputs:
    # Ra: in this case, the function anticipates having a return stream as input,
    #    rather than prices.
    # scale: number of periods per year
    # Outputs:
    # This function returns a Sterling Ratio

    # FUNCTION:

    R = checkData(R)
    if(is.na(scale)) {
        freq = periodicity(R)
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
    annualized_return = Return.annualized(R, scale=scale)
    drawdown = abs(maxDrawdown(R)+excess)
    result = annualized_return/drawdown
    rownames(result) = paste("Sterling Ratio (Excess = ", round(excess*100,0), "%)", sep="")
    return(result)
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
