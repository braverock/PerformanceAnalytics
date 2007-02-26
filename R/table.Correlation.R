`table.Correlation` <-
function (Ra, Rb, n, trim = TRUE, na.rm = FALSE, ...)
{# @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating correlation and significance against
    # each column of the data provided.

    # Inputs:
    # Assumes that Ra and Rb are sequenced exactly the same.

    # Output:
    # A data table of correlation and corresponding p-value for each column
    # in Rb.

    # FUNCTION:

    # Prepare the data

    # target.vec is the vector of data we want correlations for; we'll get it
    # from Ra
    target.vec = checkDataVector(Ra)
    # data.matrix is a vector or matrix of data we want correlations against;
    # we'll take it from Rb
    data.matrix = checkDataMatrix(Rb)
    columns=ncol(Rb)
    columnnames = colnames(Rb)

    #  For each period (or row):
    for (column in 1:columns) {
        values = vector('numeric', 0)

        # Calculate correlation and significance
        htest = cor.test(Ra, Rb[,column], ...)
        values = cbind(htest$estimate, htest$p.value,htest$conf.int[1],htest$conf.int[2])

        if(column == 1) {
            result.df = data.frame(Value = values)#, row.names = columnnames[column])
        }

        else {
            nextrow = data.frame(Value = values)#, row.names = columnnames[column])
            result.df = rbind(result.df, nextrow)
        }
    }

    colnames(result.df) = c("Correlation","p-value","Lower CI","Upper CI")
    rownames(result.df) = columnnames

#     if (trim & TS)
#         positions = positions[-(1:(n - 1))]
#     if (TS) {
#         resultingtable = timeSeries(as.matrix(ans), positions, units = x.orig@units,
#             FinCenter = x.orig@FinCenter)
#     }
    result.df

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.Correlation.R,v 1.3 2007-02-26 22:04:36 brian Exp $
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