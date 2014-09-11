#' Higher Moments Summary: Statistics and Stylized Facts
#' 
#' Summary of the higher moements and Co-Moments of the return distribution.
#' Used to determine diversification potential. Also called "systematic"
#' moments by several papers.
#' 
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param Rf risk free rate, in same period as your returns
#' @param digits number of digits to round results to
#' @param method method to use when computing \code{\link{kurtosis}} one of:
#' \code{excess}, \code{moment}, \code{fisher}
#' @author Peter Carl
#' @seealso \code{\link{CoSkewness}} \cr \code{\link{CoKurtosis}} \cr
#' \code{\link{BetaCoVariance}} \cr \code{\link{BetaCoSkewness}} \cr
#' \code{\link{BetaCoKurtosis}} \cr \code{\link{skewness}} \cr
#' \code{\link{kurtosis}}
#' @references Martellini L., Vaissie M., Ziemann V. Investing in Hedge Funds:
#' Adding Value through Active Style Allocation Decisions. October 2005. Edhec
#' Risk and Asset Management Research Centre.
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' table.HigherMoments(managers[,1:3],managers[,8,drop=FALSE])
#' result=t(table.HigherMoments(managers[,1:6],managers[,8,drop=FALSE]))
#' rownames(result)=colnames(managers[,1:6])
#' require("Hmisc")
#' textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, 
#'          cdec=rep(3,dim(result)[2])), rmar = 0.8, cmar = 1.5,  
#'          max.cex=.9, halign = "center", valign = "top", row.valign="center", 
#'          wrap.rownames=5, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
#' title(main="Higher Co-Moments with SP500 TR")
#' 
#' @export
table.HigherMoments <-
function (Ra, Rb, scale = NA, Rf = 0, digits = 4, method = "moment")
{# @author Peter Carl

    # DESCRIPTION:
    # Higher Moments Summary: Statistics and Stylized Facts
    #
    # Used to determine diversification potential.
    # Also called "systematic kurtosis" or "systematic cokurtosis" by several papers.
    #
    # as defined in
    # Martellini L., Vaissie M., Ziemann V., October 2005,
    # Investing in Hedge Funds:
    #   Adding Value through Active Style Allocation Decisions
    # Edhec Risk and Asset Management Research Centre

    # Inputs:
    # x = return vector of initial portfolio
    # y = return vector of asset being considered for addition to portfolio

    # Assumes inputs are monthly returns, do not contain NA's, and are
    # lined up correctly.

    # Outputs:
    # A table of parameters from a linear regression of excess returns

    # FUNCTION:
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    Ra.excess = Return.excess(Ra, Rf)
    Rb.excess = Return.excess(Rb, Rf)

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        for(column.b in 1:columns.b) { # against each asset passed in as Rb
            merged.assets = merge(Ra.excess[,column.a,drop=FALSE], Rb.excess[,column.b,drop=FALSE])
            merged.assets = na.omit(merged.assets) # leaves the overlapping period

            z = c(
                    CoSkewness(merged.assets[,1,drop=FALSE], merged.assets[,2,drop=FALSE]),
                    CoKurtosis(merged.assets[,1,drop=FALSE], merged.assets[,2,drop=FALSE]),
                    BetaCoVariance(merged.assets[,1,drop=FALSE], merged.assets[,2,drop=FALSE]),
                    BetaCoSkewness(merged.assets[,1,drop=FALSE], merged.assets[,2,drop=FALSE]),
                    BetaCoKurtosis(merged.assets[,1,drop=FALSE], merged.assets[,2,drop=FALSE])
                    )

            znames = c(
                    "CoSkewness",
                    "CoKurtosis",
                    "Beta CoVariance",
                    "Beta CoSkewness",
                    "Beta CoKurtosis"
                    )

            if(column.a == 1 & column.b == 1) {
                result.df = data.frame(Value = z, row.names = znames)
                colnames(result.df) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
            }
            else {
                nextcolumn = data.frame(Value = z, row.names = znames)
                colnames(nextcolumn) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
                result.df = cbind(result.df, nextcolumn)
            }
        }
    }

    result.df = base::round(result.df, digits)
    result.df
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
