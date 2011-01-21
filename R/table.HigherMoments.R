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
# Copyright (c) 2004-2011 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################