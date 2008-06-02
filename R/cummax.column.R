`cummax.column` <-
function (x)
{ # @author Peter Carl

    # NOTE: a prior version of this function included na.rm as a parameter
    # this was removed because the cumulative functions are now generic functions
    # in R.
    # Conversation with Kurt Hornik (who consulted Brian Ripley) indicated that
    # the best handling of NA's in these functions might be to do specific
    # replacement such that the returned column series would have the same length
    # as the input series.  For now, I've simply removed the offending parameters,
    # but left the code here and this note to remind us to revisit the decision.

    # to get to drawdown calculations, we need cummax.column
    x = checkDataMatrix(x)

    #if (na.rm) {
    #    result = apply(na.omit(x), MARGIN = 2, FUN = cummax, ...)
    #}
    #else {
        result = apply(x, MARGIN = 2, FUN = cummax)
    #}
    result
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: cummax.column.R,v 1.8 2008-06-02 16:05:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2007/07/14 17:24:14  brian
# - remove dots from apply to pass R CMD check
#
# Revision 1.6  2007/07/10 09:13:28  brian
# - comment out old na.rm handling to pass R CMD check
#
# Revision 1.5  2007/07/10 09:06:54  brian
# - change parameters to x only, for reconciliation with R core
#
# Revision 1.4  2007/07/09 13:42:06  brian
# - update to pass R CMD check
#
# Revision 1.3  2007/02/22 18:26:26  brian
# - update function name for consistency
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################