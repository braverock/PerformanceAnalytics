#' calculate an annualized return for comparing instruments with different
#' length history
#' 
#' An average annualized return is convenient for comparing returns.
#' 
#' Annualized returns are useful for comparing two assets.  To do so, you must
#' scale your observations to an annual scale by raising the compound return to
#' the number of periods in a year, and taking the root to the number of total
#' observations:
#' \deqn{prod(1+R_{a})^{\frac{scale}{n}}-1=\sqrt[n]{prod(1+R_{a})^{scale}}-1}{prod(1
#' + Ra)^(scale/n) - 1}
#' 
#' where scale is the number of periods in a year, and n is the total number of
#' periods for which you have observations.
#' 
#' For simple returns (geometric=FALSE), the formula is:
#' 
#' \deqn{\overline{R_{a}} \cdot scale}{mean(R)*scale}
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @author Peter Carl
#' @seealso \code{\link{Return.cumulative}},
#' @references Bacon, Carl. \emph{Practical Portfolio Performance Measurement
#' and Attribution}. Wiley. 2004. p. 6
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' Return.annualized(managers[,1,drop=FALSE])
#' Return.annualized(managers[,1:8])
#' Return.annualized(managers[,1:8],geometric=FALSE)
#' 
#' @export
Return.annualized <-
function (R, scale = NA, geometric = TRUE )
{ # @author Peter Carl

    # Description:

    # An average annualized return is convenient for comparing returns.
    # @todo: This function could be used for calculating geometric or simple
    # returns

    # R = periods under analysis
    # scale = number of periods in a year (daily f = 252, monthly f = 12,
    # quarterly f = 4)

    # arithmetic average: ra = (f/n) * sum(ri)
    # geometric average: rg = product(1 + ri)^(f/n) - 1

    # @todo: don't calculate for returns less than 1 year

    # FUNCTION:
    if (is.vector(R)) {
        R = checkData (R)
        R = na.omit(R)
        n = length(R)
        if(!xtsible(R) & is.na(scale))
            stop("'R' needs to be timeBased or xtsible, or scale must be specified." )
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
        #do the correct thing for geometric or simple returns
        if (geometric) {
            # geometric returns
            result = prod(1 + R)^(scale/n) - 1
        } else {
            # simple returns
            result = mean(R) * scale
        }
        result
    }
    else {
        R = checkData(R, method = "xts")
        result = apply(R, 2, Return.annualized, scale = scale, geometric = geometric)
        dim(result) = c(1,NCOL(R))
        colnames(result) = colnames(R)
        rownames(result) = "Annualized Return"
        return(result)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
