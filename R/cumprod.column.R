`cumprod.column` <-
function (x)
{ # @author Peter Carl

    # if we do this, then cumulating a set of monthly returns is easy
    x = checkDataMatrix(x)

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
# $Id: cumprod.column.R,v 1.4 2007-07-10 09:06:54 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/07/09 13:42:06  brian
# - update to pass R CMD check
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################