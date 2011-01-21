chart.ACF <- function(R, maxlag = NULL, elementcolor = "gray", main = NULL, ...)
{ # @author David Stoffer and Robert Shumway
    # @modifiedby Peter Carl

    # DESCRIPTION:

    # Inspired by the same charts as chart.ACFplus.R

    # From the website: http://www.stat.pitt.edu/stoffer/tsa2/Rcode/acf2.R
    # "...here's an R function that will plot the ACF and PACF of a time series 
    # at the same time on the SAME SCALE, and it leaves out the zero lag in the 
    # ACF: acf2.R. If your time series is in x and you want the ACF and PACF of 
    # x to lag 50, the call to the function is acf2(x,50). The number of lags 
    # is optional, so acf2(x) will use a default number of lags [√n + 10, where 
    # n is the number of observations]."

    # This function uses those same defaults to print just the ACF chart.

    R = checkData(R)
    data = checkData(R[,1], method="vector", na.rm = TRUE)

    columns = ncol(R)
    rows = nrow(R)
    columnnames = colnames(R)

    if(is.null(main))
        main = columnnames[1]

    num = length(data)
    if (is.null(maxlag)) 
        maxlag = ceiling(10 + sqrt(num))
    ACF = acf(data, maxlag, plot = FALSE)$acf[-1]
    Lag = 1:length(ACF)/frequency(data)
    minA = min(ACF)
    U = 2/sqrt(num)
    L = -U
    minu = min(minA, L) - .01

    plot(Lag, ACF, type = "h", ylim = c(minu,1), main = main, axes = FALSE, ...)
    box(col=elementcolor)
    axis(2, col = elementcolor, cex.axis = 0.8)
    axis(1, col = elementcolor, cex.axis = 0.8)
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))

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