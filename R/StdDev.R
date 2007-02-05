`StdDev` <-
function(R)
{ # @author Brian G. Peterson

    r = checkDataVector(R)

    # DESCRIPTION:
    # Standard deviation of Monthly Returns:
    result = sqrt(var(r))

    # Return Value:
    result
}

###############################################################################

`std` <-
function(r) {
    # NOTE: std function is listed in the doc for fBasics, but not implemented
    return(StdDev(R))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: StdDev.R,v 1.2 2007-02-05 19:03:04 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged foles to version control
# Bug 890
#
###############################################################################