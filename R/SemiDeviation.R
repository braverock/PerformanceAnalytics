`SemiDeviation` <-
function (R)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function is just a wrapper of DownsideDeviation with
    # MAR = mean(x)
    # see below

    # FUNCTION:

    x = checkDataVector(R)

    return(DownsideDeviation(R, MAR=mean(R)))
}

`SemiVariance` <-
function (R)
{
    x = checkDataVector(R)

    return(DownsideDeviation(R, MAR=mean(R)))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: SemiDeviation.R,v 1.3 2007-03-01 03:26:59 brian Exp $
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