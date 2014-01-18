#' Create an ECDF overlaid with a Normal CDF
#' 
#' Creates an emperical cumulative distribution function (ECDF) overlaid with a
#' cumulative distribution function (CDF)
#' 
#' The empirical cumulative distribution function (ECDF for short) calculates
#' the fraction of observations less or equal to a given value.  The resulting
#' plot is a step function of that fraction at each observation.  This function
#' uses \code{ecdf} and overlays the CDF for a fitted normal function as well.
#' Inspired by a chart in Ruppert (2004).
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param main set the chart title, same as in \code{\link{plot}}
#' @param xlab set the x-axis label, same as in \code{\link{plot}}
#' @param ylab set the y-axis label, same as in \code{\link{plot}}
#' @param xaxis if true, draws the x axis
#' @param yaxis if true, draws the y axis
#' @param colorset color palette to use, defaults to c("black", "\#005AFF"),
#' where first value is used to color the step function and the second color is
#' used for the fitted normal
#' @param lwd set the line width, same as in \code{\link{plot}}
#' @param element.color specify the color of chart elements.  Default is
#' "darkgray"
#' @param lty set the line type, same as in \code{\link{plot}}
#' @param \dots any other passthru parameters to \code{\link{plot}}
#' @author Peter Carl
#' @seealso \code{\link{plot}}, \code{\link{ecdf}}
#' @references Ruppert, David. \emph{Statistics and Finance, an Introduction}.
#' Springer. 2004. Ch. 2 Fig. 2.5
#' 
#' \url{http://www.stat.tamu.edu/~ljin/Finance/chapter2/Fig2_5.txt}
#' @keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(edhec)
#' chart.ECDF(edhec[, 1, drop=FALSE])
#' 
#' 
#' @export 
chart.ECDF <-
function(R, main = "Empirical CDF", xlab="x", ylab="F(x)", colorset = c("black", "#005AFF"), lwd = 1, lty = c(1,1), element.color = "darkgray", xaxis=TRUE, yaxis=TRUE, ...)
{ # @author Peter Carl

    # DESCRIPTION:

    # Create an ECDF overlaid with a CDF

    # Inputs:
    # R = usually a set of monthly return

    # Code inspired by a chart on David Rupert's web page
    # http://www.stat.tamu.edu/~ljin/Finance/chapter2/Fig2_5.txt

    R = checkData(R, method="vector", na.rm = TRUE)

    t = seq(-2.5,2.5,length.out=1000)
    p1 = pnorm(t,mean(R), sd.xts(R))

    rx = sort(R)

    stepy = ((0:(length(R)-1))+0.5)/length(R)
 
    plot(rx, stepy, type="s", col = colorset[1], xlab = xlab, ylab = ylab, main = main, axes = FALSE, lty = lty[1], lwd = lwd, ...)
    lines(t, p1, lty = lty[2], col = colorset[2], lwd = lwd, ...)

    if(xaxis)
      axis(1, cex.axis = 0.8, col = element.color)
    if (yaxis)
      axis(2, cex.axis = 0.8, col = element.color)

    box(col=element.color)

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
