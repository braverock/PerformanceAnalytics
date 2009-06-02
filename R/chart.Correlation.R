`chart.Correlation` <-
function (x, histogram = TRUE)
{ # @author R Development Core Team
  # @author modified by Peter Carl
    # Visualization of a Correlation Matrix. On top the (absolute) value of the
    # correlation plus the result of the cor.test as stars. On botttom, the
    # bivariate scatterplots, with a fitted line

    # Published at http://addictedtor.free.fr/graphiques/sources/source_137.R
    panel.cor <- function(x, y, digits=2, prefix="", use="pairwise.complete.obs", cex.cor, ...)
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- abs(cor(x, y, use = use))
        txt <- format(c(r, 0.123456789), digits=digits)[1]
        txt <- paste(prefix, txt, sep="")
        if(missing(cex.cor)) cex <- 0.8/strwidth(txt)

        test <- cor.test(x,y)
        # borrowed from printCoefmat
        Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                    symbols = c("***", "**", "*", ".", " "))

        text(0.5, 0.5, txt, cex = cex * r)
        text(.8, .8, Signif, cex=cex, col=2)
    }
    f <- function(t) {
    dnorm(t, mean=mean(x), sd=sd(x) )
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
        pairs(x, gap=0, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=hist.panel)
    else
        pairs(x, gap=0, lower.panel=panel.smooth, upper.panel=panel.cor)#, pch=".")
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Correlation.R,v 1.6 2009-06-02 03:23:09 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.4  2008/01/18 04:05:15  peter
# - fixed missing values in density
# - removed y as unnecessary parameter
#
# Revision 1.3  2007/12/27 18:44:36  peter
# - added option for passing parameters to cor, including
# "pairwise.complete.obs"
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################