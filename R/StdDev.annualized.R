`StdDev.annualized` <-
function (R, rf = 0, scale = 12)
{ # @author Peter Carl

    # To annualize standard deviation, we multiply by the square root of the
    # number of observations per year.

    R = checkDataVector(R)

    # return(sqrt(scale)*sqrt(var(x - rf)))
    return(sqrt(scale)*sqrt(var(R, na.rm = TRUE)))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: StdDev.annualized.R,v 1.3 2007-02-15 01:14:43 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################