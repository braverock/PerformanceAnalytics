

#' correlation matrix chart
#' 
#' Visualization of a Correlation Matrix. On top the (absolute) value of the
#' correlation plus the result of the cor.test as stars. On bottom, the
#' bivariate scatterplots, with a fitted line
#' 
#' 
#' @param R data for the x axis, can take matrix,vector, or timeseries
#' @param histogram TRUE/FALSE whether or not to display a histogram
#' @param method a character string indicating which correlation coefficient
#'           (or covariance) is to be computed.  One of "pearson"
#'           (default), "kendall", or "spearman", can be abbreviated.
#' @param \dots any other passthru parameters into \code{\link{pairs}}
#' @note based on plot at
#' \url{http://addictedtor.free.fr/graphiques/sources/source_137.R}
#' @author Peter Carl
#' @seealso \code{\link{table.Correlation}}
#' @keywords ts multivariate distribution models hplot
#' @examples
#' 
#' data(managers)
#' chart.Correlation(managers[,1:8], histogram=TRUE, pch="+")
#' 
#' @export 
chart.Correlation <-
function (R, histogram = TRUE, method=c("pearson", "kendall", "spearman"), ...)
{ # @author R Development Core Team
  # @author modified by Peter Carl
    # Visualization of a Correlation Matrix. On top the (absolute) value of the
    # correlation plus the result of the cor.test as stars. On botttom, the
    # bivariate scatterplots, with a fitted line

    x = checkData(R, method="matrix")
    
    if(missing(method)) method=method[1] #only use one

    # Published at http://addictedtor.free.fr/graphiques/sources/source_137.R
    panel.cor <- function(x, y, digits=2, prefix="", use="pairwise.complete.obs", method, cex.cor, ...)
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y, use=use, method=method) # MG: remove abs here
        txt <- format(c(r, 0.123456789), digits=digits)[1]
        txt <- paste(prefix, txt, sep="")
        if(missing(cex.cor)) cex <- 0.8/strwidth(txt)

        test <- cor.test(x,y)
        # borrowed from printCoefmat
        Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                    symbols = c("***", "**", "*", ".", " "))
        # MG: add abs here and also include a 30% buffer for small numbers
        text(0.5, 0.5, txt, cex = cex * (abs(r) + .3) / 1.3)
        text(.8, .8, Signif, cex=cex, col=2)
    }
    f <- function(t) {
    dnorm(t, mean=mean(x), sd=sd.xts(x) )
    }
    hist.panel = function (x, ...) {
        par(new = TRUE)
        hist(x,
             col = "light gray",
             probability = TRUE,
             axes = FALSE,
             main = "",
             breaks = "FD")
        lines(density(x, na.rm=TRUE),
              col = "red",
              lwd = 1)
        #lines(f, col="blue", lwd=1, lty=1) how to add gaussian normal overlay?
        rug(x)
      }
    # Draw the chart
    if(histogram)
        pairs(x, gap=0, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=hist.panel, method=method, ...)
    else
        pairs(x, gap=0, lower.panel=panel.smooth, upper.panel=panel.cor, method=method, ...) 
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
