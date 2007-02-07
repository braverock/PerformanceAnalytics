`chart.QQPlot` <-
function (R, colorset = (1:12), symbolset = 1, xlab = NULL, ylab = NULL, main = NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns

    # Outputs:
    # A Normal Q-Q Plot

    # FUNCTION:

    x = checkDataVector(R)

    if(is.null(xlab))
        xlab = "Normal Quantiles"

    if(is.null(ylab))
        ylab = "Empirical Quantiles"

    if(is.null(main))
        main = "Normal QQ-Plot"

    # Normal Quantile-Quantile Plot:
    qqnorm(x, xlab = xlab, ylab = ylab, main = main, pch = symbolset, ...)
    qqline(x, col = colorset)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.QQPlot.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################