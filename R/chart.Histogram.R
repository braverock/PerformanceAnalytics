`chart.Histogram` <-
function(R, rf = 0, breaks="FD", main = NULL, add.names = TRUE, xlab = "Returns", ylab = "Frequency", border.col = "white", add.density = TRUE, add.fit = TRUE, density.col = "black", fit.col = "darkgray", colorset = "gray", lwd = 2, xlim = NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:

    # Create a histogram of returns, with optional curve fits for density
    # and normal

    # Inputs:
    # R = usually a set of monthly return

    # Code inspired by a chart on:
    # http://zoonek2.free.fr/UNIX/48_R/03.html

    y = checkDataMatrix(R)
    x = checkDataVector(y[,1])

    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)

    if(is.null(main)){
        main = columnnames[1]
    }

    xlim = c(qnorm(0.001, mean(x), stdev(x)), qnorm(0.999, mean(x), stdev(x)))

    hist(x = x, probability = TRUE, xlim = xlim, col = colorset, border = border.col, xlab = xlab, main = main, breaks = breaks, ...)

    # Show Density Estimate:
    if(add.density){
        den = density(x)
        lines(den, col = density.col, lwd = lwd)
        #plot(den, xlab = xlab, ylab = ylab, xlim = xlim, lty = 3, lwd = 2, main = "Manager Returns", col = density.col)
    }

    # Add Normal Fit:
    if(add.fit){
        s = seq(xlim[1], xlim[2], length = 500)
        lines(s, dnorm(s, mean(x), stdev(x)), col = fit.col, lwd = lwd)
        # @todo: add other fits
        #lines(s, dcauchy(s,mean(x), stdev(x)))
    }

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Histogram.R,v 1.3 2007-04-15 12:56:04 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################