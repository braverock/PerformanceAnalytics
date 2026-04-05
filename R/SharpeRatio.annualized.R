#' calculate annualized Sharpe Ratio
#'
#' The Sharpe Ratio is a risk-adjusted measure of return that uses standard
#' deviation to represent risk.
#'
#' The Sharpe ratio is simply the return per unit of risk (represented by
#' variance).  The higher the Sharpe ratio, the better the combined performance
#' of "risk" and return.
#'
#' This function annualizes the number based on the scale parameter.
#'
#' \deqn{\frac{\sqrt[n]{prod(1+R_{a})^{scale}}-1}{\sqrt{scale}\cdot\sqrt{\sigma}}}
#'
#' Using an annualized Sharpe Ratio is useful for comparison of multiple return
#' streams.  The annualized Sharpe ratio is computed by dividing the annualized
#' mean monthly excess return by the annualized monthly standard deviation of
#' excess return.
#'
#' William Sharpe now recommends Information Ratio preferentially to the
#' original Sharpe Ratio.
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rf risk free rate, in same period as your returns
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default FALSE
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso \code{\link{SharpeRatio}} \cr \code{\link{InformationRatio}} \cr
#' \code{\link{TrackingError}} \cr \code{\link{ActivePremium}} \cr
#' \code{\link{SortinoRatio}}
#' @references Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio
#' Management},Fall 1994, 49-58.
### keywords ts multivariate distribution models
#' @examples
#'
#' data(managers)
#' SharpeRatio.annualized(managers[, 1, drop = FALSE], Rf = .035 / 12)
#' SharpeRatio.annualized(managers[, 1, drop = FALSE], Rf = managers[, 10, drop = FALSE])
#' SharpeRatio.annualized(managers[, 1:6], Rf = .035 / 12)
#' SharpeRatio.annualized(managers[, 1:6], Rf = managers[, 10, drop = FALSE])
#' SharpeRatio.annualized(managers[, 1:6], Rf = managers[, 10, drop = FALSE], geometric = FALSE)
#'
#' @export
SharpeRatio.annualized <-
  function(R, Rf = 0, scale = NA, geometric = FALSE, ...) { # @author Peter Carl

    if (is.na(scale)) {
      return(SharpeRatio(R = R, Rf = Rf, FUN = "StdDev", annualize = TRUE, geometric = geometric, ...))
    } else {
      return(SharpeRatio(R = R, Rf = Rf, FUN = "StdDev", annualize = TRUE, geometric = geometric, scale = scale, ...))
    }
  }

###############################################################################
# R (https://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2026 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
