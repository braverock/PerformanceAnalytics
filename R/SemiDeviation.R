`SemiDeviation` <-
function (R)
{ # @author Peter Carl

    # DESCRIPTION:
    # This function is just a wrapper of DownsideDeviation with
    # MAR = mean(x)
    # see below

    # FUNCTION:

    if (is.vector(R)) {
        R = na.omit(R)
        return(DownsideDeviation(R, MAR=mean(R), method="full"))
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, 2, SemiDeviation)
        result = matrix(result, nrow=1)
        colnames(result) = colnames(R)
        rownames(result) = "Semi-Deviation"
        return(result)
    }
}

`SemiVariance` <-
function (R)
{
    if (is.vector(R)) {
        R = na.omit(R)
        return(DownsideDeviation(R, MAR=mean(R), method="subset"))
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, 2, SemiDeviation)
        dim(result) = c(1,NCOL(R))
        colnames(result) = colnames(R)
        rownames(result) = "Semi-Variance"
        return(result)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: SemiDeviation.R,v 1.11 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.10  2009-10-06 15:14:44  peter
# - fixed rownames
# - fixed scale = 12 replacement errors
#
# Revision 1.9  2009-10-06 02:57:47  peter
# - added label to results
#
# Revision 1.8  2009-09-24 03:14:01  peter
# - added multi-column support
#
# Revision 1.7  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.6  2007/10/11 03:21:42  peter
# - fixed return so that method was being passed to DownsideDeviation
#
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