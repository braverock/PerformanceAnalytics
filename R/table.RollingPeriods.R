`table.TrailingPeriods` <-
function (R,  periods = subset(c(12,36,60), c(12,36,60)
< length(as.matrix(R[,1]))), FUNCS=c("mean","sd"), funcs.names = c("Average", "Std Dev"), digits = 4, ...)
{# @author Peter Carl

    # DESCRIPTION:
    #  Rolling Periods Summary: Statistics and Stylized Facts

    # Inputs:
    # Assumes an input of monthly returns

    # Output:
    # A table of estimates of rolling period return measures

    # FUNCTION:

    R = checkData(R)

    # Set up dimensions and labels
    columns = ncol(R)
    columnnames = colnames(R)
    freq = periodicity(R)
    freq.lab = freq$label

    if(length(FUNCS) != length(funcs.names)) {
        warning("The length of the names vector is unequal to the length of the functions vector, so using FUNCS for naming.")
        funcs.names = NA
    }
    if(is.na(funcs.names[1]))
        funcs.names = FUNCS

    # for each column in the matrix, do the following:
    for(column in 1:columns) {
        valueNames = vector('character', 0)
        values = vector('numeric', 0)
        column.data = na.omit(R[,column,drop=FALSE])

        for(FUNC in FUNCS) {
            func.name = funcs.names[grep(FUNC, FUNCS)]
            for(period in periods) {
                values = c(values, apply(as.matrix(last(column.data, period)), FUN = FUNC, ..., MARGIN = 2))
                valueNames = c(valueNames,paste("Last", period, freq.lab, func.name, sep=" "))
            }
        }
    
        if(column == 1) {
            resultingtable = data.frame(Value = values, row.names = valueNames)
        }

        else {
            nextcolumn = data.frame(Value = values, row.names = valueNames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans

}

`table.TrailingPeriodsRel` <-
function (R, Rb, periods = subset(c(12,36,60), c(12,36,60)
< length(as.matrix(R[,1]))), FUNCS=c("cor","CAPM.beta"), funcs.names = c("Correlation", "Beta"), digits = 4, ...)
{# @author Peter Carl

    # DESCRIPTION:
    #  Rolling Periods Summary: Statistics and Stylized Facts

    # Inputs:
    # Assumes an input of monthly returns

    # Output:
    # A table of estimates of rolling period return measures

    # FUNCTION:

    R = checkData(R)
Rb=checkData(Rb)

    # Set up dimensions and labels
    columns = ncol(R)
    columns.b = ncol(Rb)
    columnnames = colnames(R)
    columnnames.b = colnames(Rb)
    freq = periodicity(R)
    freq.lab = freq$label

    if(length(FUNCS) != length(funcs.names)) {
        warning("The length of the names vector is unequal to the length of the functions vector, so using FUNCS for naming.")
        funcs.names = NA
    }
    if(is.na(funcs.names[1]))
        funcs.names = FUNCS

    # for each column in the matrix, do the following:
    for(column in 1:columns) {
      for(column.b in 1:columns.b){
        valueNames = vector('character', 0)
        values = vector('numeric', 0)
        merged.data = na.omit(merge(R[,column,drop=FALSE],Rb[,column.b,drop=FALSE]))

        for(FUNC in FUNCS) {
            func.name = funcs.names[grep(FUNC, FUNCS)]
            for(period in periods) {
                values = c(values, apply(last(merged.data[,1,drop=FALSE], period), FUN = FUNC, last(merged.data[,2,drop=FALSE], period), ..., MARGIN = 2))
                valueNames = c(valueNames,paste("Last", period, freq.lab, func.name, "to", columnnames.b[column.b], sep=" "))
            }
        }
    
        if(column == 1) {
            resultingtable = data.frame(Value = values, row.names = valueNames)
        }

        else {
            nextcolumn = data.frame(Value = values, row.names = valueNames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
      }
    }

    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.8  2009-04-18 02:56:53  peter
# - argument cleanup and codoc issues
#
# Revision 1.7  2009-04-14 03:45:02  peter
# - smarter labeling of frequency scale
#
# Revision 1.6  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.5  2007/03/22 21:53:00  peter
# - added checkData
# - using zoo for window() calcs in period lengths
#
# Revision 1.4  2007/02/26 22:04:36  brian
# - changes in functions to pass "R CMD check" for package
#
# Revision 1.3  2007/02/25 18:23:40  brian
# - change call to round() to call base::round() to fix conflict with newest fCalendar
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################
