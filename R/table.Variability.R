#' Variability Summary: Statistics and Stylized Facts
#' 
#' Table of Mean absolute difference, Monthly standard deviation and annualised
#' standard deviation
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param digits number of digits to round results to
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.65
#' @seealso \code{\link{StdDev.annualized}}
#' \cr \code{\link{MeanAbsoluteDeviation}}
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' table.Variability(managers[,1:8])
#' 
#' require("Hmisc")
#' result = t(table.Variability(managers[,1:8]))
#' 
#' textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1)),
#' rmar = 0.8, cmar = 2,  max.cex=.9, halign = "center", valign = "top",
#' row.valign="center", wrap.rownames=20, wrap.colnames=10,
#' col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,3,0)+0.1)
#' title(main="Portfolio variability")
#' 
#' @export
table.Variability <-
function (R, scale = NA, geometric = TRUE, digits = 4)
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
        z = c(MeanAbsoluteDeviation(y[,column,drop=FALSE]), StdDev.annualized(y[,column,drop=FALSE], scale = scale)/sqrt(scale), StdDev.annualized(y[,column,drop=FALSE], scale = scale))

        znames = c("Mean Absolute deviation", "Monthly Std Dev", "Annualized Std Dev")


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
