`table.DownsideRisk` <-
function (R, ci = 0.95, scale = NA, Rf = 0, MAR = .1/12, p= 0.95, digits = 4)
{# @author Peter Carl

    # DESCRIPTION:
    # Downside Risk Summary: Statistics and Stylized Facts

    # Inputs:
    # R: a regular timeseries of returns (rather than prices)

    # Output:
    # A table of estimates of downside risk measures

    #FUNCTION:

    y = checkData(R, method = "zoo")
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf, method = "zoo")
    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)

    if(is.na(scale)) {
        freq = periodicity(y)
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

    # for each column, do the following:
    for(column in 1:columns) {
        x = na.omit(y[,column,drop=FALSE])
        # for each column, make sure that R and Rf are for the same dates
        if(!is.null(dim(Rf))){ # if Rf is a column
            z = merge(x,Rf)
            zz = na.omit(z)
            x = zz[,1,drop=FALSE]
            Rf.subset = zz[,2,drop=FALSE]
        }
        else { # unless Rf is a single number
            Rf.subset = Rf
        }

        z = c(
                DownsideDeviation(x,MAR=mean(x)),
                sd(subset(as.vector(x),as.vector(x)>0)),
                sd(subset(as.vector(x),as.vector(x)<0)),
                DownsideDeviation(x,MAR=MAR),
                DownsideDeviation(x,MAR=Rf.subset),
                DownsideDeviation(x,MAR=0),
                maxDrawdown(x),
                VaR(x, p=p,method="historical"),
                ES(x, p=p,method="historical"),
                VaR(x, p=p),
                ES(x, p=p)
                )
        znames = c(
                "Semi Deviation",
                "Gain Deviation",
                "Loss Deviation",
                paste("Downside Deviation (MAR=",MAR*scale*100,"%)", sep=""),
                paste("Downside Deviation (Rf=",base::round(mean(Rf.subset*scale*100),2),"%)", sep=""),
                paste("Downside Deviation (0%)", sep=""),
                "Maximum Drawdown",
                paste("Historical VaR (",p*100,"%)",sep=""),
                paste("Historical ES (",p*100,"%)",sep=""),
                paste("Modified VaR (",p*100,"%)",sep=""),
                paste("Modified ES (",p*100,"%)",sep="")
                )
        if(column == 1) {
            resultingtable = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans

    #  For example:
    #     > table.DownsideRisk(monthlyReturns.ts,Rf=.04/12)
    #                                     Actual    S&P500TR
    #     Semi Deviation                0.020849116  0.02913679
    #     Gain Deviation                0.023009623  0.01975342
    #     Loss Deviation                0.007740678  0.01344530
    #     Downside Deviation (MAR=10%)  0.019826422  0.02944389
    #     Downside Deviation (Rf=4%)    0.016275404  0.02713448
    #     Downside Deviation (0%)       0.014248969  0.02642777
    #     Maximum Drawdown             -0.052021280 -0.04080700
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.DownsideRisk.R,v 1.13 2009-10-08 19:47:59 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.12  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.11  2009-04-15 21:50:10  peter
# - fixed calculation using subset on zoo object
# - passing in column using drop=FALSE to preserve names
#
# Revision 1.10  2008-10-14 14:37:29  brian
# - convert from matrix or data.frame to zoo in checkData call
#
# Revision 1.9  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.8  2007/08/29 03:17:24  peter
# - fixed NA removal problems
# - fixed rounding issue with Rf labels in table
#
# Revision 1.7  2007/08/16 14:47:40  peter
# - added subset handling for when Rf is a time series rather than a point
# est
#
# Revision 1.6  2007/04/10 01:43:38  peter
# - cleanup to prevent warning about names from checkData
#
# Revision 1.5  2007/03/22 14:39:45  peter
# - uses checkData
#
# Revision 1.4  2007/03/04 20:59:27  brian
# - minor changes to pass R CMD check
#
# Revision 1.3  2007/02/25 18:23:40  brian
# - change call to round() to call base::round() to fix conflict with newest fCalendar
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################
