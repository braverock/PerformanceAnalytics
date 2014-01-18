#' Specific risk Summary: Statistics and Stylized Facts
#' 
#' Table of specific risk, systematic risk and total risk
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param digits number of digits to round results to
#' @author Matthieu Lestel
#' @seealso \code{\link{SystematicRisk}} \cr \code{\link{SpecificRisk}}
#' \cr \code{\link{TotalRisk}}
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.76
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' table.SpecificRisk(managers[,1:8], managers[,8])
#' 
#' require("Hmisc")
#' result = t(table.SpecificRisk(managers[,1:8], managers[,8], Rf=.04/12))
#' 
#' textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1)),
#' rmar = 0.8, cmar = 2,  max.cex=.9, halign = "center", valign = "top", 
#' row.valign="center", wrap.rownames=20, wrap.colnames=10, 
#' col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,3,0)+0.1)
#' title(main="Portfolio specific, systematic and total risk")
#' 
#' @export
table.SpecificRisk <-
function (Ra, Rb, Rf = 0, digits = 4)
{
    y = checkData(Ra)
    Rb = checkData(Rb)

    # Set up dimensions and labels
    columns = ncol(y)
    #rows = nrow(y)
    columnnames = colnames(y)
    #rownames = rownames(y)

    #set up frequency
    scale = Frequency(Ra)

    # for each column, do the following:
    for(column in 1:columns) {
        z = c(SpecificRisk(y[,column,drop=FALSE], Rb = Rb, Rf = Rf, Period = scale), SystematicRisk(y[,column,drop=FALSE], Rb = Rb, Rf = Rf, Period = scale), TotalRisk(y[,column,drop=FALSE], Rb = Rb, Rf = Rf, Period = scale))

        znames = c("Specific Risk", "Systematic Risk", "Total Risk")


        if(column == 1) {
            resultingtable = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames) 
            resultingtable = cbind(resultingtable, nextcolumn)
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
