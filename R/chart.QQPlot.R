`chart.QQPlot` <-
function (R, colorset = c("#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000"), symbolset = 1, xlab = NULL, ylab = NULL, main = NULL, darken = FALSE, distribution = "normal", line = TRUE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns

    # Outputs:
    # A Normal Q-Q Plot

    # FUNCTION:

    x = checkData(R, method = "vector", na.rm = TRUE)
    n = length(x)
    if(darken)
        elementcolor = "darkgray" #better for the printer
    else
        elementcolor = "lightgray" #better for the screen


    if(distribution == "normal") {
        if(is.null(xlab)) xlab = "Normal Quantiles"
        if(is.null(ylab)) ylab = "Empirical Quantiles"
        if(is.null(main)) main = "Normal QQ-Plot"

        # Normal Quantile-Quantile Plot:
        qqnorm(x, xlab = xlab, ylab = ylab, main = main, pch = symbolset, axes = FALSE, ...)
#         qqline(x, col = colorset[2], lwd = 2)
        q.theo = qnorm(c(0.25,0.75))
    }
    if(distribution == "sst") {
        library("sn")
        if(is.null(xlab)) xlab = "Skew-T Quantiles"
        if(is.null(ylab)) ylab = "Empirical Quantiles"
        if(is.null(main)) main = "Skew-T QQ-Plot"

        # Skew Student-T Quantile-Quantile Plot:
        y = qst(c(1:n)/(n+1))
        qqplot(y, x, xlab = xlab, ylab = ylab, axes=FALSE, ...)
        q.theo = qst(c(0.25,0.75))
    }
    if(distribution == "cauchy") {
        if(is.null(xlab)) xlab = "Cauchy Quantiles"
        if(is.null(ylab)) ylab = "Empirical Quantiles"
        if(is.null(main)) main = "Cauchy QQ-Plot"

        # Skew Student-T Quantile-Quantile Plot:
        y = qcauchy(c(1:n)/(n+1))
        qqplot(y, x, xlab = xlab, ylab = ylab, axes=FALSE, ...)
        q.theo = qcauchy(c(0.25,0.75))
    }
    if(distribution == "lnorm") {
        if(is.null(xlab)) xlab = "Log Normal Quantiles"
        if(is.null(ylab)) ylab = "Empirical Quantiles"
        if(is.null(main)) main = "Log Normal QQ-Plot"

        # Skew Student-T Quantile-Quantile Plot:
        y = qlnorm(c(1:n)/(n+1))
        qqplot(y, x, xlab = xlab, ylab = ylab, axes=FALSE, ...)
        q.theo = qlnorm(c(0.25,0.75))
    }

    q.data=quantile(x,c(0.25,0.75))
    slope = diff(q.data)/diff(q.theo)
    int = q.data[1] - slope* q.theo[1]

    if(line) abline(int, slope, col = colorset[2], lwd = 2)

    axis(1, cex.axis = 0.8, col = elementcolor)
    axis(2, cex.axis = 0.8, col = elementcolor)

    box(col=elementcolor)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.QQPlot.R,v 1.4 2007-11-21 05:31:28 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/09/26 02:59:23  peter
# - changed chart elements to be consistent with other chart
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################