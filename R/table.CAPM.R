#' Single Factor Asset-Pricing Model Summary: Statistics and Stylized Facts
#'
#' Takes a set of returns and relates them to a benchmark return. Provides a
#' set of measures related to an excess return single factor model, or CAPM.
#'
#' This table will show statistics pertaining to an asset against a set of
#' benchmarks, or statistics for a set of assets against a benchmark.
#'
#' @param Ra a vector of returns to test, e.g., the asset to be examined
#' @param Rb a matrix, data.frame, or timeSeries of benchmark(s) to test the
#' asset against.
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param Rf risk free rate, in same period as your returns
#' @param digits number of digits to round results to
#' @author Peter Carl
#' @seealso \code{\link{CAPM.alpha}} \cr \code{\link{CAPM.beta}} \cr
#' \code{\link{TrackingError}} \cr \code{\link{ActivePremium}} \cr
#' \code{\link{InformationRatio}} \cr \code{\link{TreynorRatio}}
###keywords ts multivariate distribution models
#' @examples
#'
#' data(managers)
#' table.SFM(managers[,1:3], managers[,8], Rf = managers[,10])
#'
#' result = table.SFM(managers[,1:3], managers[,8], Rf = managers[,10])
#' textplot(result, rmar = 0.8, cmar = 1.5,  max.cex=.9, 
#'          halign = "center", valign = "top", row.valign="center", 
#'          wrap.rownames=15, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
#' title(main="Single Factor Model Related Statistics")
#'
#' @rdname table.CAPM
#' @aliases
#' table.CAPM
#' table.SFM
#' @export table.SFM table.CAPM
table.SFM <- table.CAPM <- function (Ra, Rb, scale = NA, Rf = 0, digits = 4)
{# @author Peter Carl

    # Inputs:
    # Ra: a vector of returns to test, e.g., the asset to be examined
    # Rb: a matrix, data.frame, or timeSeries of benchmarks to test the asset
    #     against.

    # Assumes inputs are monthly returns, do not contain NA's, and are
    # lined up correctly.

    # Outputs:
    # A table of parameters from a linear regression of excess returns

    # FUNCTION:

    # Transform input data

    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    Ra.excess = Return.excess(Ra, Rf)
    Rb.excess = Return.excess(Rb, Rf)

    if(is.na(scale)) {
        freq = periodicity(Ra)
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

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        for(column.b in 1:columns.b) { # against each asset passed in as Rb
            merged.assets = merge(Ra.excess[,column.a,drop=FALSE], Rb.excess[,column.b,drop=FALSE])
            merged.assets = as.data.frame(na.omit(merged.assets)) # leaves the overlapping period
            # these should probably call CAPM.alpha and CAPM.beta for consistency (not performance)
            models = SFM.coefficients(merged.assets[,1] ,merged.assets[,2], method="Both", Model=T)
            model.lm = models$LS$model
            model.lmRobDetMM = models$robust$model
            
            alpha.lm = coef(model.lm)[[1]]
            beta.lm = coef(model.lm)[[2]]
            alpha.rob = coef(model.lmRobDetMM)[[1]]
            beta.rob = coef(model.lmRobDetMM)[[2]]
            
            CAPMbull.lm = CAPM.beta.bull(Ra[,column.a], Rb[,column.b], Rf) #inefficient, recalcs excess returns and intercept
            CAPMbear.lm = CAPM.beta.bear(Ra[,column.a], Rb[,column.b], Rf) #inefficient, recalcs excess returns and intercept
            
            CAPMbull.rob = CAPM.beta.bull(Ra[,column.a], Rb[,column.b], Rf, method="Robust") #inefficient, recalcs excess returns and intercept
            CAPMbear.rob = CAPM.beta.bear(Ra[,column.a], Rb[,column.b], Rf, method="Robust") #inefficient, recalcs excess returns and intercept
            
            htest = cor.test(as.numeric(merged.assets[,1]), as.numeric(merged.assets[,2]))
            #active.premium = (Return.annualized(merged.assets[,1,drop=FALSE], scale = scale) - Return.annualized(merged.assets[,2,drop=FALSE], scale = scale))
            active.premium = ActivePremium(Ra=Ra[,column.a], Rb=Rb[,column.b], scale = scale)
            #tracking.error = sqrt(sum(merged.assets[,1] - merged.assets[,2])^2/(length(merged.assets[,1])-1)) * sqrt(scale)
            tracking.error = TrackingError(Ra[,column.a], Rb[,column.b], scale=scale)
            #treynor.ratio = Return.annualized(merged.assets[,1,drop=FALSE], scale = scale)/beta
            treynor.ratio = TreynorRatio(Ra=Ra[,column.a], Rb=Rb[,column.b], Rf = Rf, scale = scale)

            z = c(
                    alpha.lm,
                    beta.lm,
                    alpha.rob,
                    beta.rob,
                    CAPMbull.lm,
                    CAPMbear.lm,
                    CAPMbull.rob,
                    CAPMbear.rob,
                    
                    summary(model.lm)$r.squared,
                    summary(model.lmRobDetMM)$r.squared,
                    ((1+alpha.lm)^scale - 1),
                    htest$estimate,
                    htest$p.value,
                    tracking.error,
                    active.premium,
                    active.premium/tracking.error,
                    treynor.ratio
                    )

            znames = c(
                    "Alpha",
                    "Beta",
                    "Alpha Robust",
                    "Beta Robust",
                    "Beta+",
                    "Beta-",
                    "Beta+ Robust",
                    "Beta- Robust",
                    "R-squared",
                    "R-squared Robust",
                    "Annualized Alpha",
                    "Correlation",
                    "Correlation p-value",
                    "Tracking Error",
                    "Active Premium",
                    "Information Ratio",
                    "Treynor Ratio"
                    )

            if(column.a == 1 & column.b == 1) {
                result.df = data.frame(Value = z, row.names = znames)
                colnames(result.df) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
            }
            else {
                nextcolumn = data.frame(Value = z, row.names = znames)
                colnames(nextcolumn) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
                result.df = cbind(result.df, nextcolumn)
            }
        }
    }

    result.df = base::round(result.df, digits)
    result.df
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2020 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
