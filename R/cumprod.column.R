`cumprod.column` <-
function (R, na.rm = TRUE, ...)
{ # @author Peter Carl

    # if we do this, then cumulating a set of monthly returns is easy
    x = checkDataMatrix(R)

    if (na.rm) {
        result = apply(na.omit(x), MARGIN = 2, FUN = cumprod,
            ...)
    }
    else {
        result = apply(x, MARGIN = 2, FUN = cumprod, ...)
    }
    result
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: cumprod.column.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################