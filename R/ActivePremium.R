#' Active Premium or Active Return
#'
#' The return on an investment's annualized return minus the benchmark's
#' annualized return.
#'
#' Active Premium = Investment's annualized return - Benchmark's annualized
#' return
#'
#' Also commonly referred to as 'active return'.
#'
#' @param Ra return vector of the portfolio
#' @param Rb return vector of the benchmark asset
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @author Peter Carl
#' @seealso \code{\link{InformationRatio}} \code{\link{TrackingError}}
#' \code{\link{Return.annualized}}
#' @references Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio
#' Management},Fall 1994, 49-58.
#' @keywords ts multivariate distribution models
#' @examples
#'
#'     data(managers)
#'     ActivePremium(managers[, "HAM1", drop=FALSE], managers[, "SP500 TR", drop=FALSE])
#'     ActivePremium(managers[,1,drop=FALSE], managers[,8,drop=FALSE])
#'     ActivePremium(managers[,1:6], managers[,8,drop=FALSE])
#'     ActivePremium(managers[,1:6], managers[,8:7,drop=FALSE])
#' @rdname ActivePremium
#' @aliases
#' ActivePremium
#' ActiveReturn
#' @export
ActiveReturn <- ActivePremium <- function (Ra, Rb, scale = NA)
{ # @author Peter Carl

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

    ap <-function (Ra, Rb, scale)
    {
        merged = na.omit(merge(Ra, Rb)) # align
        ap = (Return.annualized(merged[,1], scale = scale) - Return.annualized(merged[,2], scale = scale))
        ap
    }

    result = apply(pairs, 1, FUN = function(n, Ra, Rb, scale) ap(Ra[,n[1]], Rb[,n[2]], scale), Ra = Ra, Rb = Rb, scale = scale)

    if(length(result) ==1)
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Active Premium:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
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
