`table.HigherMoments` <-
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
    Ra = checkData(Ra, method = "zoo")
    Rb = checkData(Rb, method = "zoo")
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf, method = "zoo")

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
                    CoSkewness(merged.assets[,1], merged.assets[,2]),
                    CoKurtosis(merged.assets[,1], merged.assets[,2]),
                    BetaCoVariance(merged.assets[,1], merged.assets[,2]),
                    BetaCoSkewness(merged.assets[,1], merged.assets[,2]),
                    BetaCoKurtosis(merged.assets[,1], merged.assets[,2])
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
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.HigherMoments.R,v 1.10 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.9  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.8  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.7  2007/08/16 14:48:43  peter
# - added checkData for Rf
#
# Revision 1.6  2007/03/22 13:49:42  peter
# - cleaned up comments
#
# Revision 1.5  2007/03/22 11:40:08  peter
# - handles multiple assets and benchmarks
# - uses checkData
#
# Revision 1.4  2007/02/25 18:23:40  brian
# - change call to round() to call base::round() to fix conflict with newest fCalendar
#
# Revision 1.3  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.2  2007/02/07 13:20:52  brian
# - change Ri to Rb for benchmark asset to standardize parameters
# - change indexReturns.vec to benchmarkReturns.vec for consistency
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################