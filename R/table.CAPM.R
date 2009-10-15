`table.CAPM` <-
function (Ra, Rb, scale = NA, Rf = 0, digits = 4)
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

    # Assumes inputs are monthly returns, do not contain NA's, and are
    # lined up correctly.

    # Outputs:
    # A table of parameters from a linear regression of excess returns

    # FUNCTION:

    # Transform input data

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

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        for(column.b in 1:columns.b) { # against each asset passed in as Rb
            merged.assets = merge(Ra.excess[,column.a,drop=FALSE], Rb.excess[,column.b,drop=FALSE])
            merged.assets = as.data.frame(na.omit(merged.assets)) # leaves the overlapping period
            model.lm = lm(merged.assets[,1] ~ merged.assets[,2])
            alpha = coef(model.lm)[[1]]
            beta = coef(model.lm)[[2]]
            htest = cor.test(merged.assets[,1], merged.assets[,2])
            active.premium = (Return.annualized(merged.assets[,1,drop=FALSE], scale = scale) - Return.annualized(merged.assets[,2,drop=FALSE], scale = scale))
            tracking.error = sqrt(sum(merged.assets[,1] - merged.assets[,2])^2/(length(merged.assets[,1])-1)) * sqrt(scale)
            treynor.ratio = Return.annualized(merged.assets[,1,drop=FALSE], scale = scale)/beta
    
            z = c(
                    alpha,
                    beta,
                    CAPM.beta.bull(merged.assets[,1,drop=FALSE], merged.assets[,2,drop=FALSE]), #inefficient
                    CAPM.beta.bear(merged.assets[,1,drop=FALSE], merged.assets[,2,drop=FALSE]), #inefficient
                    summary(model.lm)$r.squared,
                    ((1+alpha)^scale - 1),
                    htest$estimate,
                    htest$p.value,
                    tracking.error,
                    active.premium,
                    active.premium/tracking.error,
                    treynor.ratio
                    )
        
            znames = c(
                    "Alpha",
                    "Beta",
                    "Beta+",
                    "Beta-",
                    "R-squared",
                    "Annualized Alpha",
                    "Correlation",
                    "Correlation p-value",
                    "Tracking Error",
                    "Active Premium",
                    "Information Ratio",
                    "Treynor Ratio"
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
# $Id: table.CAPM.R,v 1.16 2009-10-15 03:37:08 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.15  2009-10-10 12:40:08  brian
# - update copyright to 2004-2009
#
# Revision 1.14  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.13  2009-10-02 19:17:08  peter
# - added up and down market beta measures
#
# Revision 1.12  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.11  2007/03/22 13:48:51  peter
# - removed and edited comments
#
# Revision 1.10  2007/03/22 11:34:12  peter
# - changed the colname label connector from "vs" to "to"
#
# Revision 1.9  2007/03/22 01:24:17  peter
# - shortened col labels
#
# Revision 1.8  2007/03/22 01:03:16  peter
# - handles uneven periods of data
# - handles matrixes of assets and benchmarks
# - uses checkData
#
# Revision 1.7  2007/03/02 17:41:48  brian
# - remove redundant comments
#
# Revision 1.6  2007/02/28 03:22:39  peter
# - added checkDataVector function to Rf
#
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