`chart.Histogram` <-
function(R, breaks="FD", main = NULL, xlab = "Returns", ylab = "Frequency", methods = c("none","add.density", "add.normal", "add.centered", "add.cauchy", "add.sst", "add.rug", "add.risk", "add.qqplot"), show.outliers = TRUE, colorset = c("lightgray", "#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000"), border.col = "white", lwd = 2, xlim = NULL, ylim = NULL, elementcolor="gray", note.lines = NULL, note.labels = NULL, note.cex = 0.7, note.color = "darkgray", probability = FALSE, p=0.99, ...)
{ # @author Peter Carl

    # DESCRIPTION:

    # Create a histogram of returns, with optional curve fits for density
    # and normal

    # Inputs:
    # R = usually a set of monthly return

    # Code inspired by a chart on:
    # http://zoonek2.free.fr/UNIX/48_R/03.html

    y = checkData(R)
    x = checkData(na.omit(y[,1]), method="vector")

    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    n=length(x)
    rangedata = 0

    if(is.null(main)){
        main = columnnames[1]
    }

    if(is.null(methods) || methods[1]=="none"){
        methods = NULL
    }

    b = c(-VaR.CornishFisher(x,p=p),-VaR.traditional(x,p=p))
    b.labels = c(paste(p*100,"% ModVaR",sep=" "),paste(p*100,"% VaR",sep=""))
#     xlim = range(qnorm(0.001, mean(x), stdev(x)), qnorm(0.999, mean(x), stdev(x)), note.lines, b)
    if(show.outliers)
        rangedata = c(min(x),max(x))
    else
        rangedata =  c(qnorm(0.001, mean(x), stdev(x)), qnorm(0.999, mean(x), stdev(x)))
    if(!is.null(note.lines)) {
        rangedata = c(rangedata,note.lines)
    }
    yrange = 0
    if(is.null(xlim))
        xlim = range(rangedata)

     s = seq(xlim[1], xlim[2], length = 500)
#     s = seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length = 500)

    # Things to do before the plot is drawn
    for (method in methods) {
        switch(method,
            add.density = {
                # Show density estimate
                den = density(x, n=length(x))
                yrange=c(yrange,max(den$y))
                 probability = TRUE
            },
            add.stable = {
                stopifnot("package:fBasics" %in% search() || require("fBasics",quietly=TRUE))
                fit.stable = stableFit(x,doplot = FALSE)
                fitted.stable = dstable(s,alpha = fit.stable@fit$estimate[[1]], beta = fit.stable@fit$estimate[[2]], gamma = fit.stable@fit$estimate[[3]], delta = fit.stable@fit$estimate[[4]], pm = 0)
                # look at documentation for pm
                yrange=c(yrange,max(fitted.stable))
                probability = TRUE
            },
            add.cauchy = {
                # requires library(MASS)
                stopifnot("package:MASS" %in% search() || require("MASS",quietly=TRUE))

                # This uses a Maximum Likelihood method as shown on:
                # Wessa P., (2006), Maximum-likelihood Cauchy Distribution Fitting (v1.0.0) in
                # Free Statistics Software (v1.1.21-r4), Office for Research Development and
                # Education, URL http://www.wessa.net/rwasp_fitdistrcauchy.wasp/
                fit = fitdistr(x, 'cauchy')
                xlab = paste("Cauchy (location = ",round(fit$estimate[[1]],2),", scale = ",round(fit$estimate[[2]],2),")", sep="")
                fitted.cauchy = dcauchy(s,location = fit$estimate[[1]], scale = fit$estimate[[2]], log = FALSE)
                yrange=c(yrange,max(fitted.cauchy))
                probability = TRUE
            },
            add.sst = {
#               requires library(sn)
                stopifnot("package:sn" %in% search() || require("sn",quietly=TRUE))

                fit = st.mle(y=x)
                fitted.sst = dst(s, location = fit$dp[[1]], scale = fit$dp[[2]], shape = fit$dp[[3]], df=fit$dp[[4]], log = FALSE)
                yrange=c(yrange,max(fitted.sst))
                probability = TRUE
            },
            add.lnorm = {
                fit = fitdistr(1+x,'log-normal')
                fitted.lnorm = dlnorm(1+s, meanlog = fit$estimate[[1]], sdlog = fit$estimate[[2]], log = FALSE)
                yrange=c(yrange,max(fitted.lnorm))
                probability = TRUE
            },
            add.normal = {
                fitted.normal = dnorm(s, mean(x), stdev(x))
                yrange=c(yrange,max(fitted.normal))
                probability = TRUE
            },
            add.centered = {
                fitted.centered = dnorm(s, 0, stdev(x))
                yrange=c(yrange,max(fitted.centered))
                probability = TRUE
            },
            add.risk = {
                rangedata = c(rangedata,b)
            }
        )
    }

    # Draw the plot
    if(probability == TRUE) maxyhist = max(hist(x, breaks = breaks, plot = FALSE)$density)
    else maxyhist = max(hist(x, breaks = breaks, plot = FALSE)$count)
    yrange = c(yrange, maxyhist*1.1)
    ylim = c(0,ceiling(max(yrange)))

    hist(x = x, probability = probability, xlim = xlim, ylim = ylim, col = colorset[1], border = border.col, xlab = xlab, main = main, breaks = breaks, axes = FALSE, ...)
    axis(1, col = elementcolor)
    axis(2, col = elementcolor)

    box(col=elementcolor)

    # Things to do after the plot is drawn
    for (method in methods) {
        switch(method,
            add.density = {
                # Show density estimate
                lines(den, col = colorset[2], lwd = lwd)
            },
            add.normal = {
                # Show normal distribution around the mean
                lines(s, fitted.normal, col = colorset[3], lwd = lwd)
            },
            add.centered = {
                # Show normal distribution around 0
                lines(s, fitted.centered, col = colorset[3], lwd = lwd)
            },
            add.lnorm = {
                # Show normal distribution around the mean
                lines(s, fitted.lnorm, col = colorset[4], lwd = lwd)
            },
            add.cauchy = {
                lines(s, fitted.cauchy, col = colorset[4], lwd=lwd)
            },
            add.stable = {
                lines(s, fitted.stable, col = colorset[4], lwd=lwd)
            },
            add.sst = { #requires package sn
                lines(s, fitted.sst, col = colorset[4], lwd=lwd)
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
                 qqnorm(x, xlab="", ylab="", main="", axes=FALSE, pch=".",col=colorset[2])
                 qqline(x, col=colorset[3])
                 box(col=elementcolor)
             }
        ) # end switch
    } # end for

    # Draw and label arbitrary lines
    if(!is.null(note.lines)) {
        #number.note.labels = ((length(note.labels)-length(note.ind) + 1):length(note.labels))

        abline(v = note.lines, col = note.color, lty = 2)
        if(!is.null(note.labels)) {
            h = rep(.2*par("usr")[3] + 0.99*par("usr")[4], length(b))
            text(note.lines, h, note.labels, offset = .2, pos = 2, cex = note.cex, srt = 90, col = note.color)

        }
    }
#                 abline(v = b, col = "darkgray", lty=2)
#                 text(b, h, b.labels, offset = .2, pos = 2, cex = 0.8, srt=90)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Histogram.R,v 1.33 2008-06-25 03:31:13 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.32  2008-06-23 02:35:10  peter
# - added note line text size attribute
# - added check for 'sn' library
#
# Revision 1.31  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.30  2008/01/15 21:06:13  peter
# - fixed ylim for probability T or F
#
# Revision 1.28  2007/12/29 19:25:09  brian
# - minor changes to pass R CMD check
#
# Revision 1.27  2007/12/27 20:11:19  peter
# - fixed F for FALSE in function call
#
# Revision 1.24  2007/11/23 04:28:01  peter
# - added margin to histogram bars for ylim
#
# Revision 1.17  2007/11/19 03:40:46  peter
# - smoothed out the density line for smaller data sets
# - added parameter for showing all data points rather than center
#
# Revision 1.16  2007/09/26 03:33:12  peter
# - no longer clobbers xlim when passed in from function
#
# Revision 1.14  2007/09/18 03:24:07  peter
# - default for methods is now NULL
#
# Revision 1.13  2007/09/14 02:04:56  peter
# - commented need for adding MASS as a dependency
#
# Revision 1.12  2007/08/24 04:02:51  peter
# - labels now work for note.lines
#
# Revision 1.11  2007/08/24 03:54:54  peter
# - added arbitrary lines and labels
# - labeling doesn't work yet
#
# Revision 1.10  2007/08/24 03:18:08  peter
# - added cauchy fit
#
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
