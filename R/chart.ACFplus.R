#' @rdname chart.ACF
#' @export 
chart.ACFplus <- function(R, maxlag = NULL, elementcolor = "gray", main = NULL, ...)
{ # @author David Stoffer and Robert Shumway
    # @modifiedby Peter Carl

    # DESCRIPTION:


    # From the website: http://www.stat.pitt.edu/stoffer/tsa2/Rcode/acf2.R
    # "...here's an R function that will plot the ACF and PACF of a time series 
    # at the same time on the SAME SCALE, and it leaves out the zero lag in the 
    # ACF: acf2.R. If your time series is in x and you want the ACF and PACF of 
    # x to lag 50, the call to the function is acf2(x,50). The number of lags 
    # is optional, so acf2(x) will use a default number of lags [√n + 10, where 
    # n is the number of observations]."

    R = checkData(R)
    data = checkData(R[,1, drop=FALSE], rm.na=TRUE, method="vector")

    columns = ncol(R)
    rows = nrow(R)
    columnnames = colnames(R)

    if(is.null(main))
        main = columnnames[1]

    num = length(data)
    if (is.null(maxlag)) 
        maxlag = ceiling(10 + sqrt(num))
    ACF = acf(data, maxlag, plot = FALSE)$acf[-1]
    PACF = pacf(data, maxlag, plot = FALSE)$acf
    Lag = 1:length(ACF)/frequency(data)
    minA = min(ACF)
    minP = min(PACF)
    U = 2/sqrt(num)
    L = -U
    minu = min(minA, minP, L) - .01

    op <- par(no.readonly=TRUE)

    layout(rbind(1,2))
#     par(mfrow = c(2,1), mar = c(3,3,2,0.8), oma = c(1,1.2,1,1), mgp = c(1.5,0.6,0))
    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # ACF chart
    par(mar=c(0.5,4,4,2) + 0.1)
    plot(Lag, ACF, type = "h", ylim = c(minu,1), main = main, axes = FALSE, ...)
    box(col=elementcolor)
    axis(2, col = elementcolor, cex.axis = 0.8)
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))

    # PACF chart
    par(mar=c(4,4,0.5,2)+ 0.1)
    plot(Lag, PACF, type = "h", ylim = c(minu,1), axes = FALSE, ...)
    box(col=elementcolor)
    axis(1, col = elementcolor, cex.axis = 0.8)
    axis(2, col = elementcolor, cex.axis = 0.8)
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))

    par(op)
#    return(cbind(ACF, PACF)) 
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################