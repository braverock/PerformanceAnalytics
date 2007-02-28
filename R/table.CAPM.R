`table.CAPM` <-
function (Ra, Rb, scale = 12, rf = 0, digits = 4)
{# @author Peter Carl

    # DESCRIPTION:
    # Asset-Pricing Model Summary: Statistics and Stylized Facts
    #
    # Takes a set of returns and relates them to a market benchmark.
    # Provides a set of measures related to the excess return single index
    # model, or CAPM.

    # Inputs:
    # Ra: a vector of returns to test, e.g., the asset to be examined
    # Rb: a matrix, data.frame, or timeSeries of benchmarks to test the asset
    #     against.

    # This is irritating, but if you want to pass in a sub-set of a timeSeries,
    # you need to use
    # > seriesData(monthlyReturns.ts[,2])
    # rather than
    # > monthlyReturns.ts@Data[,2]
    # since the latter will strip the column names.

    # Assumes inputs are monthly returns, do not contain NA's, and are
    # lined up correctly.

    # Outputs:
    # A table of parameters from a linear regression of excess returns

    # FUNCTION:

    # Prepare the data
    # target.vec is the vector of data we want correlations for; we'll get it
    # from x
    assetReturns.vec = checkDataVector(Ra)
    rf.vec = checkDataVector(rf)

    # Make these excess returns
    assetExcessRet.vec = assetReturns.vec - rf.vec

    # data.matrix is a vector or matrix of data we want correlations against;
    # we'll take it from y
    indexes.matrix = checkDataMatrix(Rb)
    columns=ncol(indexes.matrix)
    columnnames = colnames(indexes.matrix)
    if (is.null(columnnames))
        stop("Column names are missing.  If you want to pass in a sub-set of a timeSeries, you need to use seriesData(monthlyReturns.ts[,2]) to preserve the column names.")

    # for each column in the matrix, do the following:
    for(column in 1:columns) {

        benchmarkReturns.vec = as.vector(indexes.matrix[,column])
        benchmarkReturns.vec.length = length(benchmarkReturns.vec)
        benchmarkReturns.vec = benchmarkReturns.vec[!is.na(benchmarkReturns.vec)]
        benchmarkReturns.vec.na = benchmarkReturns.vec.length - length(benchmarkReturns.vec)

        # make these excess returns, too
        indexExcessRet.vec = benchmarkReturns.vec - rf.vec

        # a few calculations
        model.lm = lm(assetExcessRet.vec ~ indexExcessRet.vec)

        alpha = coef(model.lm)[[1]]
        beta = coef(model.lm)[[2]]
        htest = cor.test(assetReturns.vec, benchmarkReturns.vec)
        ActivePremium = (Return.annualized(assetReturns.vec, scale = scale) - Return.annualized(benchmarkReturns.vec, scale = scale))
        TrackingError = sqrt(sum(assetReturns.vec - benchmarkReturns.vec)^2/(length(assetReturns.vec)-1)) * sqrt(scale)
        treynorRatio = (Return.annualized(assetReturns.vec, scale = scale) - Return.annualized(rf.vec,scale=scale))/beta

    z = c(
            alpha,
            beta,
            summary(model.lm)$r.squared,
            ((1+alpha)^scale - 1),
            htest$estimate,
            htest$p.value,
            TrackingError,
            ActivePremium,
            ActivePremium/TrackingError,
            treynorRatio
            )

    znames = c(
            "Alpha",
            "Beta",
            "R-squared",
            "Annualized Alpha",
            "Correlation",
            "Correlation p-value",
            "Tracking Error",
            "Active Premium",
            "Information Ratio",
            "Treynor Ratio"
            )

        if(column == 1) {
            result.df = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames)
            result.df = cbind(result.df, nextcolumn)
        }
    }
    colnames(result.df) = columnnames
    result.df = base::round(result.df, digits)
    result.df

    #  For example:
    # > CAPMSummary(monthlyReturns.ts@Data[, 1],seriesData(monthlyReturns.ts[, 2]))
    #                     S&P500TR
    # Alpha                 0.0065
    # Beta                  0.2909
    # R-squared             0.0987
    # Annualized Alpha      0.0809
    # Correlation           0.3142
    # Correlation p-value   0.0547
    # Tracking Error        0.0149
    # Active Premium       -0.0083
    # Information Ratio    -0.5606
    # Treynor Ratio         0.3959

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.CAPM.R,v 1.6 2007-02-28 03:22:39 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2007/02/26 22:04:36  brian
# - changes in functions to pass "R CMD check" for package
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