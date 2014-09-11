#' calculate a multiperiod or annualized Standard Deviation
#' 
#' Standard Deviation of a set of observations \eqn{R_{a}} is given by:
#' 
#' \deqn{\sigma = variance(R_{a}) , std=\sqrt{\sigma} }{std = sqrt(var(R))}
#' 
#' It should follow that the variance is not a linear function of the number of
#' observations.  To determine possible variance over multiple periods, it
#' wouldn't make sense to multiply the single-period variance by the total
#' number of periods: this could quickly lead to an absurd result where total
#' variance (or risk) was greater than 100%.  It follows then that the total
#' variance needs to demonstrate a decreasing period-to-period increase as the
#' number of periods increases. Put another way, the increase in incremental
#' variance per additional period needs to decrease with some relationship to
#' the number of periods. The standard accepted practice for doing this is to
#' apply the inverse square law. To normalize standard deviation across
#' multiple periods, we multiply by the square root of the number of periods we
#' wish to calculate over. To annualize standard deviation, we multiply by the
#' square root of the number of periods per year.
#' 
#' \deqn{\sqrt{\sigma}\cdot\sqrt{periods}}
#' 
#' Note that any multiperiod or annualized number should be viewed with
#' suspicion if the number of observations is small.
#' 
#' 
#' @aliases sd.multiperiod sd.annualized StdDev.annualized
#' @param x an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param \dots any other passthru parameters
#' @author Brian G. Peterson
#' @seealso \code{\link[stats]{sd}} \cr
#' \url{http://wikipedia.org/wiki/inverse-square_law}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 27 \cr
###keywords ts multivariate distribution models
#' @examples
#' 
#'     data(edhec)
#'     sd.annualized(edhec)
#'     sd.annualized(edhec[,6,drop=FALSE])
#'     # now for three periods:
#'     sd.multiperiod(edhec[,6,drop=FALSE],scale=3)
#' 
#' @export StdDev.annualized sd.annualized sd.multiperiod
#' @aliases StdDev.annualized sd.annualized sd.multiperiod
#' @rdname StdDev.annualized
StdDev.annualized <- sd.annualized <- sd.multiperiod <-
function (x, scale = NA, ...)
{
    if(is.na(scale) && !xtsible(x))
        stop("'x' needs to be timeBased or xtsible, or scale must be specified." )
    
    if(is.na(scale)) {
        freq = periodicity(x)
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
    
    if (is.vector(x)) {
        #scale standard deviation by multiplying by the square root of the number of periods to scale by
        sqrt(scale)*sd(x, na.rm=TRUE)
    } else { 
        if(!xtsible(x) & is.na(scale))
            stop("'x' needs to be timeBased or xtsible, or scale must be specified." )
        x = checkData (x)
                
        result = apply(x, 2, sd.multiperiod, scale=scale)
        
        dim(result) = c(1,NCOL(x))
        colnames(result) = colnames(x)
        rownames(result) = "Annualized Standard Deviation"
        return(result)
    }
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
