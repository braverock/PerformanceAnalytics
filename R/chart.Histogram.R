`chart.Histogram` <-
function(R, breaks="FD", main = NULL, add.names = TRUE, xlab = "Returns", ylab = "Frequency", border.col = "white", box.col = "white", methods = c("add.density", "add.normal", "add.centered", "add.rug", "add.risk", "add.qqplot"), colorset = c("lightgray", "#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000"), lwd = 2, xlim = NULL, darken = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:

    # Create a histogram of returns, with optional curve fits for density
    # and normal

    # Inputs:
    # R = usually a set of monthly return

    # Code inspired by a chart on:
    # http://zoonek2.free.fr/UNIX/48_R/03.html

# IDEAS
# add other fits, e.g., cauchy
# add VaR, ModifiedVaR, as vertical lines
# color the axes
# differentiate mean and zero normal fits?

    y = checkDataMatrix(R)
    x = checkDataVector(y[,1])

    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)

    if(is.null(main)){
        main = columnnames[1]
    }

    if(darken)
        elementcolor = "darkgray" #better for the printer
    else
        elementcolor = "lightgray" #better for the screen

    b = c(-VaR.CornishFisher(x),-VaR.traditional(x))
    b.labels = c("ModVaR","VaR")
    xlim = range(qnorm(0.001, mean(x), stdev(x)), qnorm(0.999, mean(x), stdev(x)), b)

    hist(x = x, probability = TRUE, xlim = xlim, col = colorset[1], border = border.col, xlab = xlab, main = main, breaks = breaks, ...)

    box(col=box.col)

    for (method in methods) {
        switch(method,
            add.density = {
                # Show density estimate
                den = density(x)
                lines(den, col = colorset[2], lwd = lwd)
            },
            add.normal = {
                # Show normal distribution around the mean
                s = seq(xlim[1], xlim[2], length = 500)
                lines(s, dnorm(s, mean(x), stdev(x)), col = colorset[3], lwd = lwd)
            },
            add.centered = {
                # Show normal distribution around 0
                s = seq(xlim[1], xlim[2], length = 500)
                lines(s, dnorm(s, 0, stdev(x)), col = colorset[3], lwd = lwd)
            },
            add.cauchy = {
                s = quantile(x)
                lines(s, dcauchy(s,location = 0, scale = 1, log = FALSE), col = colorset[4])
            },
            add.rug = {
                rug(x, col = elementcolor)
            },
            add.risk = {
                h = rep(.2*par("usr")[3] + .8*par("usr")[4], length(b))
                points(b, h, type='h', col='red',lwd=3)
                points(b, h, col='red', lwd=3)
                text(b, h, b.labels, pos=3)
            },
            add.qqplot = {
                op <- par(fig=c(.02,.5,.5,.98), new=TRUE)
                qqnorm(x, xlab="", ylab="", main="", axes=FALSE, pch=".")
                qqline(x, col="red")
                box()
            }
        ) # end switch
    } # end for

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Histogram.R,v 1.7 2007-06-07 23:42:00 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.6  2007/04/30 12:51:38  peter
# - fixed F instead of FALSE error
#
# Revision 1.5  2007/04/27 03:25:00  peter
# - added risk lines
#
# Revision 1.4  2007/04/27 03:08:22  peter
# - added switch
# - added qqchart
# - added rug
#
# Revision 1.3  2007/04/15 12:56:04  brian
# - add breaks as an explicit parameter
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################
