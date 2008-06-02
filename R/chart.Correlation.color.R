`chart.Correlation.color` <-
function (R, nrgcols = 50, labels = TRUE, labcols = 1, title = "", use="pairwise.complete.obs", ...)
{ # @author Sandrine Dudoit, sandrine@stat.berkeley.edu, from "SMA" library
  # @author modified by Peter Carl

    # Description:

    # Another, simpler, visualization of a Correlation Matrix, probably better
    # for larger sets of data.

    # Inputs:

    # R: data to be evaluated against its own members
    # new:
    # nrgcols:
    # labels:
    # labcols:
    # title:

    # Outputs:

    # A color graphic of a correlation matrix, with relative colors to indicate the
    # strength of the pairwise correlation.

    # @todo: accomodate larger labels for the columns

    rgcolors.func = function (n = 50)
    {
        k <- round(n/2)
        r <- c(rep(0, k), seq(0, 1, length = k))
        g <- c(rev(seq(0, 1, length = k)), rep(0, k))
        res <- rgb(r, g, rep(0, 2 * k))
        res
    }

    x = cor(R, use = use)

    n <- ncol(x)
    corr <- x
    image(1:n, 1:n, corr[, n:1], col = rgcolors.func(nrgcols),
        axes = FALSE, xlab = "", ylab = "", ...)
    if (length(labcols) == 1) {
        axis(2, at = n:1, labels = labels, las = 2, cex.axis = 0.6,
            col.axis = labcols)
        axis(3, at = 1:n, labels = labels, las = 2, cex.axis = 0.6,
            col.axis = labcols)
    }
    if (length(labcols) == n) {
        cols <- unique(labcols)
        for (i in 1:length(cols)) {
            which <- (1:n)[labcols == cols[i]]
            axis(2, at = (n:1)[which], labels = labels[which],
                las = 2, cex.axis = 0.6, col.axis = cols[i])
            axis(3, at = which, labels = labels[which], las = 2,
                cex.axis = 0.6, col.axis = cols[i])
        }
    }
    mtext(title, side = 3, line = 3)
    box()
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Correlation.color.R,v 1.5 2008-06-02 16:05:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2007/12/27 20:19:06  brian
# - remove unused parameter 'new'
#
# Revision 1.3  2007/12/27 18:45:24  peter
# - added parameter for passing to cor
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################