#' Distributions Summary: Statistics and Stylized Facts
#' 
#' Table of Monthly standard deviation, Skewness, Sample standard deviation,
#' Kurtosis, Excess kurtosis, Sample Skweness and Sample excess kurtosis 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param digits number of digits to round results to
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.87
#' @seealso \code{\link{StdDev.annualized}} \cr \code{\link{skewness}} \cr
#' \code{\link{kurtosis}}
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' table.Distributions(managers[,1:8])
#' 
#' require("Hmisc")
#' result = t(table.Distributions(managers[,1:8]))
#' 
#' textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1)),
#' rmar = 0.8, cmar = 2,  max.cex=.9, halign = "center", valign = "top",
#' row.valign="center", wrap.rownames=20, wrap.colnames=10,
#' col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,3,0)+0.1)
#' title(main="Portfolio Distributions statistics")
#' 
#' @export
table.Distributions <-
function (R, scale = NA, digits = 4)
{
    y = checkData(R)

    # Set up dimensions and labels
    columns = ncol(y)
    #rows = nrow(y)
    columnnames = colnames(y)
    #rownames = rownames(y)

    #set up frequency
    if(is.na(scale)) {
        freq = periodicity(R)
        switch(freq$scale,
            minute = {stop("Data periodicity too high")},
            hourly = {stop("Data periodicity too high")},
            daily = {scale = 252},
            weekly = {scale = 52},
            monthly = {scale = 12},
            quarterly = {scale = 4},
            yearly = {scale = 1}
        )
    }

    # for each column, do the following:
    for(column in 1:columns) {
        z = c(StdDev.annualized(y[,column,drop=FALSE])/sqrt(scale), skewness(y[,column,drop=FALSE], method = "moment"), kurtosis(y[,column,drop=FALSE], method = "moment"), kurtosis(y[,column,drop=FALSE], method = "excess"), skewness(y[,column,drop=FALSE], method = "sample"), kurtosis(y[,column,drop=FALSE], method = "sample_excess"))

        znames = c("Monthly Std Dev", "Skewness",  "Kurtosis", "Excess kurtosis", "Sample skewness", "Sample excess kurtosis")


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
