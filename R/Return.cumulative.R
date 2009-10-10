`Return.cumulative` <-
function (R, geometric = TRUE)
{ # @author Peter Carl

    # This is a useful function for calculating cumulative return over a period
    # of time, say a calendar year.  Can produce simple or geometric return.

    if (is.vector(R)) {
        R = na.omit(R)
        if (!geometric)
            return(sum(R))
        else {
            return(prod(1+R)-1)
        }
    }
    else {
        R = checkData(R, method = "matrix")
        result = apply(R, 2, Return.cumulative, geometric = geometric)
        dim(result) = c(1,NCOL(R))
        colnames(result) = colnames(R)
        rownames(result) = "Cumulative Return"
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
# $Id: Return.cumulative.R,v 1.8 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2009-10-06 15:14:44  peter
# - fixed rownames
# - fixed scale = 12 replacement errors
#
# Revision 1.6  2009-10-06 02:58:41  peter
# - added label to results
#
# Revision 1.5  2009-09-24 02:42:31  peter
# - added multicolumn support
#
# Revision 1.4  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.3  2007/03/11 19:18:50  brian
# - standardize variable naming
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################