#' Downside Summary: Statistics and ratios
#' 
#' Table of Monthly downside risk, Annualised downside risk, Downside potential,
#' Omega, Sortino ratio, Upside potential, Upside potential ratio and
#' Omega-Sharpe ratio
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param MAR Minimum Acceptable Return, in the same periodicity as your
#' returns
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' @param digits number of digits to round results to
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.98
#' @seealso \code{\link{CalmarRatio}} \cr \code{\link{BurkeRatio}}
#' \cr \code{\link{PainIndex}} \cr \code{\link{UlcerIndex}} \cr 
#' \code{\link{PainRatio}} \cr \code{\link{MartinRatio}}
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' table.DownsideRiskRatio(managers[,1:8])
#' 
#' require("Hmisc")
#' result = t(table.DownsideRiskRatio(managers[,1:8]))
#' 
#' textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1)),
#' rmar = 0.8, cmar = 2,  max.cex=.9, halign = "center", valign = "top",
#' row.valign="center", wrap.rownames=20, wrap.colnames=10,
#' col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,3,0)+0.1)
#' title(main="Downside risk statistics")
#' 
#' @export
table.DownsideRiskRatio <-
function (R, MAR = 0, scale = NA, digits = 4)
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
        z = c(DownsideDeviation(y[,column,drop=FALSE], MAR = MAR), DownsideDeviation(y[,column,drop=FALSE], MAR = MAR)*sqrt(scale), DownsidePotential(y[,column,drop=FALSE], MAR = MAR), UpsideRisk(y[,column,drop=FALSE], MAR = MAR, stat = "potential")/DownsidePotential(y[,column,drop=FALSE], MAR = MAR), SortinoRatio(y[,column,drop=FALSE], MAR = MAR), UpsideRisk(y[,column,drop=FALSE], MAR = MAR, stat = "potential"), UpsidePotentialRatio(y[,column,drop=FALSE], MAR = MAR), OmegaSharpeRatio(y[,column,drop=FALSE], MAR = MAR))

        znames = c("Monthly downside risk", "Annualised downside risk", "Downside potential", "Omega", "Sortino ratio", "Upside potential", "Upside potential ratio", "Omega-sharpe ratio")
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
