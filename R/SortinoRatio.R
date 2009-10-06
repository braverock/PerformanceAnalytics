`SortinoRatio` <-
function (R, MAR = 0)
{ # @author Brian G. Peterson
  # modified from function by Sankalp Upadhyay <sankalp.upadhyay [at] gmail [dot] com> with permission

    # Description:
    # Sortino proposed to better account for skill and excess peRformance
    # by using only downside semivariance as the measure of risk.

    # R     return vector
    # MAR   minimum acceptable return
    # Function:
    R = checkData(R)

    sr <-function (R, MAR)
    {
        SR = mean(Return.excess(R, MAR), na.rm=TRUE)/DownsideDeviation(R, MAR)
        SR
    }

    result = apply(R, MARGIN = 2, sr, MAR = MAR)
    dim(result) = c(1,NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = paste("Sortino Ratio (MAR = ", round(MAR,1),"%)", sep="")
    return (result)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: SortinoRatio.R,v 1.9 2009-10-06 15:14:44 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.8  2009-10-06 02:55:59  peter
# - added label to results
#
# Revision 1.7  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.6  2009-10-01 01:46:19  peter
# - added multi-column support
#
# Revision 1.5  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.4  2007/04/09 03:31:50  peter
# - uses checkData
#
# Revision 1.3  2007/03/14 00:54:06  brian
# - updates to parameters for standardization
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################