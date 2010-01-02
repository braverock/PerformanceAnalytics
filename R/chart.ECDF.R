`chart.ECDF` <-
function(R, main = "Empirical CDF", xlab="x", ylab="F(x)", colorset = c("black", "#005AFF"), lwd = 1, xlim = NULL, ylim = NULL, lty = c(1,1), element.color = "darkgray", ...)
{ # @author Peter Carl

    # DESCRIPTION:

    # Create an ECDF overlaid with a CDF

    # Inputs:
    # R = usually a set of monthly return

    # Code inspired by a chart on David Rupert's web page
    # http://www.stat.tamu.edu/~ljin/Finance/chapter2/Fig2_5.txt

    R = checkData(R, method="vector", na.rm = TRUE)

    t = seq(-2.5,2.5,length.out=1000)
    p1 = pnorm(t,mean(R), sd(R))

    rx = sort(R)

    stepy = ((0:(length(R)-1))+0.5)/length(R)
 
    plot(rx, stepy, type="s", col = colorset[1], xlab = xlab, ylab = ylab, main = main, axes = FALSE, lty = lty[1], lwd = lwd, ...)
    lines(t, p1, lty = lty[2], col = colorset[2], lwd = lwd, ...)

    axis(1, cex.axis = 0.8, col = element.color)
    axis(2, cex.axis = 0.8, col = element.color)

    box(col=element.color)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2009-04-07 22:19:46  peter
# - changed to use element.color parameter
#
# Revision 1.2  2008-06-25 03:48:00  peter
# - added copyright and CVS log
#
#
###############################################################################