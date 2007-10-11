`SemiDeviation` <-
function (Ra)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function is just a wrapper of DownsideDeviation with
    # MAR = mean(x)
    # see below

    # FUNCTION:

    Ra = checkDataVector(Ra)

    return(DownsideDeviation(Ra, MAR=mean(Ra)), method="full")
}

`SemiVariance` <-
function (Ra)
{
    Ra = checkDataVector(Ra)

    return(DownsideDeviation(Ra, MAR=mean(Ra), method="subset"))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: SemiDeviation.R,v 1.6 2007-10-11 03:21:42 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2007/08/03 14:58:26  brian
# - add use of length of full series or subset below MAR
# - set proper values for SemiVariance(subset), and SemiDeviation(full)
# - allow DownsideDeviation user to choose, default method="full"
#
# Revision 1.4  2007/03/14 00:54:06  brian
# - updates to parameters for standardization
#
# Revision 1.3  2007/03/01 03:26:59  brian
# - add function and documentation for DownsideDeviation wrapper SemiVariance
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################