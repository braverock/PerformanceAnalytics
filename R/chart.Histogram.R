#' histogram of returns
#' 
#' Create a histogram of returns, with optional curve fits for density and
#' normal.  This is a wrapper function for \code{\link[graphics]{hist}}, see
#' the help for that function for additional arguments you may wish to pass in.
#' 
#' The default for \code{breaks} is \code{"FD"}. Other names for which
#' algorithms are supplied are \code{"Sturges"} (see
#' \code{\link{nclass.Sturges}}), \code{"Scott"}, and \code{"FD"} /
#' \code{"Freedman-Diaconis"} (with corresponding functions
#' \code{\link{nclass.scott}} and \code{\link{nclass.FD}}).  Case is ignored
#' and partial matching is used.  Alternatively, a function can be supplied
#' which will compute the intended number of breaks as a function of \code{R}.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param breaks one of: 
#' \itemize{ 
#'  \item a vector giving the breakpoints between histogram cells, 
#'  \item a single number giving the number of cells for the histogram, 
#'  \item a character string naming an algorithm to compute the number of cells (see \sQuote{Details}), 
#'  \item a function to compute the number of cells.  
#' } 
#' For the last three the number is a suggestion only.
#' see \code{\link[graphics]{hist}} for details, default "FD"
#' @param methods what to graph, one or more of: 
#' \itemize{ 
#'   \item add.density to display the density plot 
#'   \item add.normal  to display a fitted normal distibution line over the mean 
#'   \item add.centered to display a fitted normal line over zero 
#'   \item add.rug to display a rug of the observations
#'   \item add.risk to display common risk metrics 
#'   \item add.qqplot to display a small qqplot in the upper corner of the histogram plot 
#' }
#' @param p confidence level for calculation, default p=.99
#' @param probability logical; if TRUE, the histogram graphic is a
#' representation of frequencies, the counts component of the result; if FALSE,
#' probability densities, component density, are plotted (so that the histogram
#' has a total area of one). Defaults to TRUE if and only if breaks are
#' equidistant (and probability is not specified). see
#' \code{\link[graphics]{hist}}
#' @param show.outliers logical; if TRUE (the default), the histogram will show
#' all of the data points.  If FALSE, it will show only the first through the
#' fourth quartile and will exclude outliers.
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param ylab set the y-axis label, same as in \code{\link{plot}}
#' @param xlab set the x-axis label, same as in \code{\link{plot}}
#' @param ylim set the y-axis limits, same as in \code{\link{plot}}
#' @param border.col color to use for the border
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}
#' @param lwd set the line width, same as in \code{\link{plot}}
#' @param colorset color palette to use, set by default to rational choices
#' @param element.color provides the color for drawing chart elements, such as
#' the box lines, axis lines, etc. Default is "darkgray"
#' @param note.lines draws a vertical line through the value given.
#' @param note.labels adds a text label to vertical lines specified for
#' note.lines.
#' @param note.cex The magnification to be used for note line labels relative
#' to the current setting of 'cex'.
#' @param note.color specifies the color(s) of the vertical lines drawn.
#' @param cex.legend The magnification to be used for sizing the legend
#' relative to the current setting of 'cex'.
#' @param cex.axis The magnification to be used for axis annotation relative to
#' the current setting of 'cex', same as in \code{\link{plot}}.
#' @param cex.lab The magnification to be used for x- and y-axis labels
#' relative to the current setting of 'cex'.
#' @param cex.main The magnification to be used for the main title relative to
#' the current setting of 'cex'.
#' @param xaxis if true, draws the x axis
#' @param yaxis if true, draws the y axis
#' @param \dots any other passthru parameters to \code{\link{plot}}
#' @note Code inspired by a chart on: \cr
#' \url{http://zoonek2.free.fr/UNIX/48_R/03.html}
#' @author Peter Carl
#' @seealso \code{\link[graphics]{hist}}
#' @keywords ts multivariate distribution models hplot
#' @examples
#' 
#'     data(edhec)
#'     chart.Histogram(edhec[,'Equity Market Neutral',drop=FALSE])
#' 
#'     # version with more breaks and the 
#' 	   # standard close fit density distribution
#'     chart.Histogram(edhec[,'Equity Market Neutral',drop=FALSE], 
#' 			breaks=40, methods = c("add.density", "add.rug") )
#' 
#'     chart.Histogram(edhec[,'Equity Market Neutral',drop=FALSE], 
#' 			methods = c( "add.density", "add.normal") )
#' 
#'     # version with just the histogram and 
#'     # normal distribution centered on 0
#'     chart.Histogram(edhec[,'Equity Market Neutral',drop=FALSE], 
#' 			methods = c( "add.density", "add.centered") )
#' 
#'     # add a rug to the previous plot 
#' 	   # for more granularity on precisely where the distribution fell
#'     chart.Histogram(edhec[,'Equity Market Neutral',drop=FALSE], 
#' 			methods = c( "add.centered", "add.density", "add.rug") )
#' 
#'     # now show a qqplot to give us another view 
#'     # on how normal the data are
#'     chart.Histogram(edhec[,'Equity Market Neutral',drop=FALSE], 
#' 			methods = c("add.centered","add.density","add.rug","add.qqplot"))
#' 
#'     # add risk measure(s) to show where those are 
#' 	   # in relation to observed returns
#'     chart.Histogram(edhec[,'Equity Market Neutral',drop=FALSE], 
#' 			methods = c("add.density","add.centered","add.rug","add.risk"))
#' 
#' @export
chart.Histogram <-
function(R, 
		breaks="FD", 
		main = NULL, 
		xlab = "Returns", 
		ylab = "Frequency", 
		methods = c("none","add.density","add.normal", 
					"add.centered","add.cauchy","add.sst",
					"add.rug","add.risk","add.qqplot"), 
		show.outliers = TRUE, 
		colorset = c("lightgray","#00008F","#005AFF",
					"#23FFDC","#ECFF13","#FF4A00","#800000"), 
		border.col = "white", 
		lwd = 2, 
		xlim = NULL, 
		ylim = NULL, 
		element.color="darkgray", 
		note.lines = NULL, 
		note.labels = NULL, 
		note.cex = 0.7, 
		note.color = "darkgray", 
		probability = FALSE, 
		p = 0.95, 
		cex.axis = 0.8, 
		cex.legend = 0.8, 
		cex.lab = 1, 
		cex.main = 1, 
		xaxis=TRUE, 
		yaxis=TRUE, 
		...
)
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

#     xlim = range(qnorm(0.001, mean(x), stdev(x)), qnorm(0.999, mean(x), stdev(x)), note.lines, b)
    if(show.outliers)
        rangedata = c(min(x),max(x))
    else
        rangedata =  c(qnorm(0.001, mean(x), sd.xts(x)), qnorm(0.999, mean(x), sd.xts(x)))
    if(!is.null(note.lines)) {
        rangedata = c(rangedata,note.lines)
    }

    if("add.risk" %in% methods){
        # TODO Add ES methods here
        b = c(VaR(x,p=p,method="modified",invert=TRUE),VaR(x,p=p,method="historical",invert=TRUE))
        b.labels = c(paste(p*100,"% ModVaR",sep=" "),paste(p*100,"% VaR",sep=""))
        rangedata = c(rangedata,b)
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
#             add.stable = {
#                 stopifnot("package:fBasics" %in% search() || require("fBasics",quietly=TRUE))
#                 fit.stable = stableFit(x,doplot = FALSE)
#                 fitted.stable = dstable(s,alpha = fit.stable@fit$estimate[[1]], beta = fit.stable@fit$estimate[[2]], gamma = fit.stable@fit$estimate[[3]], delta = fit.stable@fit$estimate[[4]], pm = 0)
#                 # look at documentation for pm
#                 yrange=c(yrange,max(fitted.stable))
#                 probability = TRUE
#             },
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
#               requires library(gamlss)
                stopifnot("package:gamlss" %in% search() || require("gamlss",quietly=TRUE))              
                fit = gamlss(coredata(y)~1, family="ST1", verbose=FALSE)
                fitted.sst = dST1(s, mu = fitted(fit)[1], sigma = fitted(fit, "sigma")[1], nu = fitted(fit, "nu")[1], tau = fitted(fit, "tau")[1])
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
                fitted.normal = dnorm(s, mean(x), sd.xts(x))
                yrange=c(yrange,max(fitted.normal))
                probability = TRUE
            },
            add.centered = {
                fitted.centered = dnorm(s, 0, sd.xts(x))
                yrange=c(yrange,max(fitted.centered))
                probability = TRUE
            },
            add.risk = {
                #
            }
        )
    }

    # Draw the plot
    if(probability == TRUE) maxyhist = max(hist(x, breaks = breaks, plot = FALSE)$density)
    else maxyhist = max(hist(x, breaks = breaks, plot = FALSE)$count)
    yrange = c(yrange, maxyhist*1.1)
	if(is.null(ylim))
		ylim = c(0,ceiling(max(yrange)))

    hist(x = x, probability = probability, xlim = xlim, ylim = ylim, col = colorset[1], border = border.col, xlab = xlab, main = main, breaks = breaks, axes = FALSE, cex.main = cex.main, cex.lab = cex.lab, ...)
    if(xaxis)
        axis(1, cex.axis = cex.axis, col = element.color)
    if(yaxis)
        axis(2, cex.axis = cex.axis, col = element.color)

    box(col=element.color)

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
#             add.stable = {
#                 lines(s, fitted.stable, col = colorset[4], lwd=lwd)
#             },
            add.sst = { #requires package sn
                lines(s, fitted.sst, col = colorset[4], lwd=lwd)
#               curve(fitted.sst, col=colorset[4], lwd=lwd, add=TRUE)
            },
            add.rug = {
                rug(x, col = element.color)
            },
            add.risk = {
                h = rep(.2*par("usr")[3] + 1*par("usr")[4], length(b))
#                points(b, h, type='h', col='red',lwd=3)
#                points(b, h, col='red', lwd=3)
                abline(v = b, col = "darkgray", lty=2)
                text(b, h, b.labels, offset = .2, pos = 2, cex = 0.8, srt=90)
            },
             add.qqplot = {
                op <- par(no.readonly=TRUE)
                op1 <- par(fig=c(.02,.5,.5,.98), new=TRUE)
                qqnorm(x, xlab="", ylab="", main="", axes=FALSE, pch=".",col=colorset[2])
                qqline(x, col=colorset[3])
                box(col=element.color)
                par(op)
             }
        ) # end switch
    } # end for

    # Draw and label arbitrary lines
    if(!is.null(note.lines)) {
        #number.note.labels = ((length(note.labels)-length(note.ind) + 1):length(note.labels))

        abline(v = note.lines, col = note.color, lty = 2)
        if(!is.null(note.labels)) {
            h = rep(.2*par("usr")[3] + 0.99*par("usr")[4], length(note.lines))
            text(note.lines, h, note.labels, offset = .2, pos = 2, cex = note.cex, srt = 90, col = note.color)

        }
    }
#                 abline(v = b, col = "darkgray", lty=2)
#                 text(b, h, b.labels, offset = .2, pos = 2, cex = 0.8, srt=90)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
