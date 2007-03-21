`table.Drawdowns` <-
function (R, top = 5)
{# @author Peter Carl

    # DESCRIPTION
    # Worst Drawdowns Summary: Statistics and Stylized Facts

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Creates a data.frame of the worst "n" drawdowns

    # FUNCTION:

    R = checkData(R, method = "zoo")

    x = sortDrawdowns(findDrawdowns(R))

    result = data.frame(time(R)[x$from[1:top]], time(R)[x$to[1:top]], x$return[1:top], x$length[1:top])

    colnames(result) = c("From", "To", "Return", "Length")

    result
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.Drawdowns.R,v 1.1 2007-03-21 04:13:43 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################