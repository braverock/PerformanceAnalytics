#' calculate Sortino Ratio of performance over downside risk
#'
#' Sortino proposed an improvement on the Sharpe Ratio to better account for
#' skill and excess performance by using only downside semivariance as the
#' measure of risk.
#'
#' Sortino contends that risk should be measured in terms of not meeting the
#' investment goal.  This gives rise to the notion of \dQuote{Minimum
#' Acceptable Return} or MAR.  All of Sortino's proposed measures include the
#' MAR, and are more sensitive to downside or extreme risks than measures that
#' use volatility(standard deviation of returns) as the measure of risk.
#'
#' Choosing the MAR carefully is very important, especially when comparing
#' disparate investment choices.  If the MAR is too low, it will not adequately
#' capture the risks that concern the investor, and if the MAR is too high, it
#' will unfavorably portray what may otherwise be a sound investment.  When
#' comparing multiple investments, some papers recommend using the risk free
#' rate as the MAR.  Practitioners may wish to choose one MAR for consistency,
#' several standardized MAR values for reporting a range of scenarios, or a MAR
#' customized to the objective of the investor.
#'
#' \deqn{ SortinoRatio=\frac{(\overline{R_{a} - MAR})}{\delta_{MAR}} } where
#' \eqn{\delta_{MAR}} is the \code{\link{DownsideDeviation}}.
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param MAR Minimum Acceptable Return, in the same periodicity as your
#' returns
#' @param \dots any other passthru parameters. This include two types of parameters.
#' The first type is parameters associated with the risk/performance measure, such as tail
#' probability for VaR and ES. The second type is the parameters associated with the metohd
#' used to compute the standard error. See \code{\link{SE.IF.iid}}, \code{\link{SE.IF.cor}},
#' \code{\link{SE.BOOT.iid}}, \code{\link{SE.BOOT.cor}} for details.
#' @param weights portfolio weighting vector, default NULL
#' @param se.method a character string indicating which method should be used to compute
#' the standard error of the estimated standard deviation. One of \code{"none"} (default),
#' \code{"IFiid"}, \code{"IFcor"}, \code{"BOOTiid"}, \code{"BOOTcor"}. Currently, it works
#' only when \code{method="historical"} and \code{portfolio_method="single"}.
#' @author @author Xin Chen, \email{chenx26@uw.edu}
#' @seealso \code{\link{SharpeRatio}} \cr \code{\link{DownsideDeviation}} \cr
#' \code{\link{SemiVariance}} \cr \code{\link{SemiDeviation}} \cr
#' \code{\link{InformationRatio}}
#' @references Sortino, F. and Price, L. Performance Measurement in a Downside
#' Risk Framework. \emph{Journal of Investing}. Fall 1994, 59-65.
###keywords ts multivariate distribution models
#' @examples
#'
#' data(managers)
#' round(SortinoRatio(managers[, 1]),4)
#' round(SortinoRatio(managers[, 1:8]),4)
#'
#' @export
SortinoRatio.SE <-
  function (R, ...,weights=NULL, se.method = "none", MAR = 0)
  { # @author Brian G. Peterson and Xin Chen
    # modified from function by Sankalp Upadhyay <sankalp.upadhyay [at] gmail [dot] com> with permission

    # Description:
    # Sortino proposed to better account for skill and excess peRformance
    # by using only downside semivariance as the measure of risk.

    # R     return vector
    # MAR   minimum acceptable return
    # Function:

    mySoR = SortinoRatio(R = R,  ..., weights = weights, MAR = MAR)

    R = checkData(R)

    #if we have a weights vector, use it
    if(!is.null(weights)){
      R=Return.portfolio(R,weights,...)
    }

    if(se.method!="none"){
      res=list(SoR=mySoR)
      # for each of the method specified in se.method, compute the standard error
      for(mymethod in se.method){
        res[[mymethod]]=EstimatorSE(R, estimator.fun = "SoR", se.method = mymethod, const = MAR)
      }
      return(res)
    } else {
      return(mySoR)
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
