`table.Drawdowns` <-
function (R, top = 5, digits = 4)
{# @author Peter Carl

    # DESCRIPTION
    # Worst Drawdowns Summary: Statistics and Stylized Facts

    # Inputs:
    # R: Assumes returns rather than prices
    # top: the number of drawdowns to include

    # Output:
    # Creates a data.frame of the worst "n" drawdowns

    # FUNCTION:

    R = checkData(R[,1,drop=FALSE])
    R = na.omit(R)
    x = sortDrawdowns(findDrawdowns(R))

    ndrawdowns = sum(x$return < 0)
    if (ndrawdowns < top){
        warning(paste("Only ",ndrawdowns," available in the data.",sep=""))
        top = ndrawdowns
    }

    result = data.frame(time(R)[x$from[1:top]], time(R)[x$trough[1:top]], time(R)[x$to[1:top]], base::round(x$return[1:top], digits), x$length[1:top], x$peaktotrough[1:top], ifelse(is.na(time(R)[x$to[1:top]]), NA, x$recovery[1:top]))

    colnames(result) = c("From", "Trough", "To", "Depth", "Length", "To Trough", "Recovery")
    subset(result,Depth<0)
    result
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.Drawdowns.R,v 1.10 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.9  2009-10-06 03:02:22  peter
# - modified to accept only one column
#
# Revision 1.8  2009-04-17 04:09:52  peter
# - parameter cleanup
#
# Revision 1.7  2009-04-14 04:24:20  peter
# - now subsets drawdowns less than zero
# - digits for formatting WDD
#
# Revision 1.6  2009-03-31 04:21:03  peter
# - fixed error when NAs in data shifted time index
# - added NAs in table when series ends in drawdown
#
# Revision 1.5  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.4  2007/04/04 00:23:01  brian
# - typos and minor comment updates
#
# Revision 1.3  2007/03/21 14:07:04  peter
# - added trough date, periods to trough, and periods to recovery
#
# Revision 1.2  2007/03/21 04:20:11  peter
# - added error trap where top exceeds # drawdowns
#
# Revision 1.1  2007/03/21 04:13:43  peter
# - initial addition of function to cvs
#
###############################################################################