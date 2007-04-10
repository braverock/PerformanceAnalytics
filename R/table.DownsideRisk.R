`table.DownsideRisk` <-
function (R, ci = 0.95, scale = 12, rf = 0, MAR = .1/12, p= 0.99, digits = 4)
{# @author Peter Carl

    # DESCRIPTION:
    # Downside Risk Summary: Statistics and Stylized Facts

    # Inputs:
    # R: a regular timeseries of returns (rather than prices)

    # Output:
    # A table of estimates of downside risk measures

    #FUNCTION:

    y = checkData(R, method="matrix")

    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)

    # for each column, do the following:
    for(column in 1:columns) {
        x = y[,column]
        x.length = length(x)
        x = x[!is.na(x)]
        x.na = x.length - length(x)
        z = c(
                DownsideDeviation(x,MAR=mean(x)),
                sd(subset(x,x>0)),
                sd(subset(x,x<0)),
                DownsideDeviation(x,MAR=MAR),
                DownsideDeviation(x,MAR=rf),
                DownsideDeviation(x,MAR=0),
                maxDrawdown(x),
                VaR.traditional(x, p=p),
                VaR.Beyond(x, p=p),
                VaR.CornishFisher(x, p=p)
                )
        znames = c(
                "Semi Deviation",
                "Gain Deviation",
                "Loss Deviation",
                paste("Downside Deviation (MAR=",MAR*scale*100,"%)", sep=""),
                paste("Downside Deviation (rf=",rf*scale*100,"%)", sep=""),
                paste("Downside Deviation (0%)", sep=""),
                "Maximum Drawdown",
                paste("VaR (",p*100,"%)",sep=""),
                "Beyond VaR",
                paste("Modified VaR (",p*100,"%)",sep="")
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
    #     > table.DownsideRisk(monthlyReturns.ts,rf=.04/12)
    #                                     Actual    S&P500TR
    #     Semi Deviation                0.020849116  0.02913679
    #     Gain Deviation                0.023009623  0.01975342
    #     Loss Deviation                0.007740678  0.01344530
    #     Downside Deviation (MAR=10%)  0.019826422  0.02944389
    #     Downside Deviation (rf=4%)    0.016275404  0.02713448
    #     Downside Deviation (0%)       0.014248969  0.02642777
    #     Maximum Drawdown             -0.052021280 -0.04080700
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.DownsideRisk.R,v 1.6 2007-04-10 01:43:38 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
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