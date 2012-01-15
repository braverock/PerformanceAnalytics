table.Drawdowns <-
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
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################