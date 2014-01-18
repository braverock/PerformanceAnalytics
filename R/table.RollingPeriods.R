#' Rolling Periods Summary: Statistics and Stylized Facts
#' 
#' A table of estimates of rolling period return measures
#' 
#' 
#' @aliases table.TrailingPeriods table.TrailingPeriodsRel table.RollingPeriods
#' 
#' @export 
#' 
#' @rdname table.RollingPeriods
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb an xts, vector, matrix, data frame, timeSeries or zoo object of
#' index, benchmark, portfolio, or secondary asset returns to compare against
#' @param periods number of periods to use as rolling window(s), subset of
#' \code{c(3, 6, 9, 12, 18, 24, 36, 48)}
#' @param funcs.names vector of function names used for labeling table rows
#' @param FUNCS list of functions to apply the rolling period to
#' @param digits number of digits to round results to
#' @param \dots any other passthru parameters for functions specified in FUNCS
#' @author Peter Carl
#' @seealso \code{\link[zoo]{rollapply}}
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' #table.TrailingPeriods(edhec[,10:13], FUNCS=c("SharpeRatio","VaR"), funcs.names = c("Sharpe Ratio", "Modified VaR"), periods=c(12,24,36))
#' 
#' #result=table.TrailingPeriods(edhec[,10:13], FUNCS=c("SharpeRatio","VaR"), funcs.names = c("Sharpe Ratio", "Modified VaR"), periods=c(12,24,36))
#' table.TrailingPeriods(edhec[,10:13], periods=c(12,24,36))
#' 
#' result=table.TrailingPeriods(edhec[,10:13], periods=c(12,24,36))
#' require("Hmisc")
#' textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(3,dim(result)[2])), rmar = 0.8, cmar = 1.5,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=15, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
#' title(main="Trailing Period Statistics")
#' 
#' @export
table.TrailingPeriods <-
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

#' @rdname table.RollingPeriods
table.TrailingPeriodsRel <-
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
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
