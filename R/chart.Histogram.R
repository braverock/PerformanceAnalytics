`chart.Histogram` <-
function(R, breaks="FD", main = NULL, xlab = "Returns", ylab = "Frequency", methods = c("add.density", "add.normal", "add.centered", "add.rug", "add.risk", "add.qqplot"), colorset = c("lightgray", "#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000"), border.col = "white", box.col = "white", lwd = 2, xlim = NULL, ylim = NULL, darken = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:

    # Create a histogram of returns, with optional curve fits for density
    # and normal

    # Inputs:
    # R = usually a set of monthly return

    # Code inspired by a chart on:
    # http://zoonek2.free.fr/UNIX/48_R/03.html

# IDEAS
# add other fits?
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

# Need to add ylim calc up here to capture full cauchy density function, otherwise it won't plot
# What else needs to be done up here before plotting the histogram?
    for (method in methods) {
        switch(method,
            add.cauchy = {
                library(MASS)
                fitted=fitdistr(x, 'cauchy')
                xlab = paste("Cauchy(location=",round(fitted$estimate[[1]],2),", scale=",round(fitted$estimate[[2]],2),")")
                s = quantile(x, probs=seq(0, 1, 0.005))
                ylim=range(dcauchy(s,location = fitted$estimate[[1]], scale = fitted$estimate[[2]], log = FALSE))
            }
        )
    }

    hist(x = x, probability = TRUE, xlim = xlim, ylim = ylim, col = colorset[1], border = border.col, xlab = xlab, main = main, breaks = breaks, ...)

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
                # First we have to fit the data to calculate the location and scale parameters (see ylim calc above)
                # This uses a Maximum Likelihood method as shown on:
                # Wessa P., (2006), Maximum-likelihood Cauchy Distribution Fitting (v1.0.0) in 
                # Free Statistics Software (v1.1.21-r4), Office for Research Development and 
                # Education, URL http://www.wessa.net/rwasp_fitdistrcauchy.wasp/

                lines(s, dcauchy(s,location = fitted$estimate[[1]], scale = fitted$estimate[[2]], log = FALSE), col = colorset[4], lwd=lwd)
            },
            add.rug = {
                rug(x, col = elementcolor)
            },
            add.risk = {
                h = rep(.2*par("usr")[3] + 1*par("usr")[4], length(b))
#                points(b, h, type='h', col='red',lwd=3)
#                points(b, h, col='red', lwd=3)
                abline(v = b, col = "darkgray", lty=2)
                text(b, h, b.labels, offset = .2, pos = 2, cex = 0.8, srt=90)
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
# $Id: chart.Histogram.R,v 1.10 2007-08-24 03:18:08 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.9  2007/08/24 01:43:04  peter
# - beautified format of vertical lines for add.risk
#
# Revision 1.8  2007/06/17 21:42:34  brian
# - update /usage and /items to pass check
#
# Revision 1.7  2007/06/07 23:42:00  brian
# - add comments
#
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
