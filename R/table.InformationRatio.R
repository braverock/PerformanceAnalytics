#' Information ratio Summary: Statistics and Stylized Facts
#' 
#' Table of Tracking error, Annualised tracking error and Information ratio
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param digits number of digits to round results to
#' @author Matthieu Lestel
#' @seealso \code{\link{InformationRatio}}
#' \cr \code{\link{TrackingError}}
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.81
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' table.InformationRatio(managers[,1:8], managers[,8])
#' 
#' require("Hmisc")
#' result = t(table.InformationRatio(managers[,1:8], managers[,8]))
#' 
#' textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1)),
#' rmar = 0.8, cmar = 2,  max.cex=.9, halign = "center", valign = "top",
#' row.valign="center", wrap.rownames=20, wrap.colnames=10,
#' col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,3,0)+0.1)
#' title(main="Portfolio information ratio")
#' 
#' @export
table.InformationRatio <-
function (R, Rb, scale = NA, digits = 4)
{
    y = checkData(R)
    Rb = checkData(Rb)

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
        z = c(TrackingError(y[,column,drop=FALSE],Rb)/sqrt(scale), TrackingError(y[,column,drop=FALSE],Rb), InformationRatio(y[,column,drop=FALSE],Rb))

        znames = c("Tracking Error", "Annualised Tracking Error", "Information Ratio")


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
