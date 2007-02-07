`Return.cumulative` <-
function (R, geometric = TRUE)
{ # @author Peter Carl

    # This is a useful function for calculating cumulative return over a period
    # of time, say a calendar year.  Can produce simple or geometric return.

    x = checkDataVector(R)

    if (!geometric)
        return(sum(x, na.rm = TRUE))
    else {
        return(prod(1+x, na.rm = TRUE)-1)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.cumulative.R,v 1.2 2007-02-07 13:24:49 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################