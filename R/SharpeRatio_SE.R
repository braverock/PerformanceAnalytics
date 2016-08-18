#' calculate a traditional or modified Sharpe Ratio of Return over StdDev or
#' VaR or ES
#'
#' The Sharpe ratio is simply the return per unit of risk (represented by
#' variability).  In the classic case, the unit of risk is the standard
#' deviation of the returns.
#'
#' \deqn{\frac{\overline{(R_{a}-R_{f})}}{\sqrt{\sigma_{(R_{a}-R_{f})}}}}
#'
#' William Sharpe now recommends \code{\link{InformationRatio}} preferentially
#' to the original Sharpe Ratio.
#'
#' The higher the Sharpe ratio, the better the combined performance of "risk"
#' and return.
#'
#' As noted, the traditional Sharpe Ratio is a risk-adjusted measure of return
#' that uses standard deviation to represent risk.
#'
#' A number of papers now recommend using a "modified Sharpe" ratio using a
#' Modified Cornish-Fisher VaR or CVaR/Expected Shortfall as the measure of
#' Risk.
#'
#' We have recently extended this concept to create multivariate modified
#' Sharpe-like Ratios for standard deviation, Gaussian VaR, modified VaR,
#' Gaussian Expected Shortfall, and modified Expected Shortfall. See
#' \code{\link{VaR}} and \code{\link{ES}}.  You can pass additional arguments
#' to \code{\link{VaR}} and \code{\link{ES}} via \dots{} The most important is
#' probably the 'method' argument/
#'
#' This function returns a traditional or modified Sharpe ratio for the same
#' periodicity of the data being input (e.g., monthly data -> monthly SR)
#'
#'
#' @aliases SharpeRatio.modified SharpeRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rf risk free rate, in same period as your returns
#' @param p confidence level for calculation, default p=.95
#' @param FUN one of "StdDev" or "VaR" or "ES" to use as the denominator
#' @param weights portfolio weighting vector, default NULL, see Details in
#' \code{\link{VaR}}
#' @param annualize if TRUE, annualize the measure, default FALSE
#' @param \dots any other passthru parameters. This include two types of parameters.
#' The first type is parameters associated with the risk/performance measure, such as tail
#' probability for VaR and ES. The second type is the parameters associated with the metohd
#' used to compute the standard error. See \code{\link{SE.IF.iid}}, \code{\link{SE.IF.cor}},
#' \code{\link{SE.BOOT.iid}}, \code{\link{SE.BOOT.cor}} for details.
#' @param se.method a character string indicating which method should be used to compute
#' the standard error of the estimated standard deviation. One of \code{"none"} (default),
#' \code{"IFiid"}, \code{"IFcor"}, \code{"BOOTiid"}, \code{"BOOTcor"}. Currently, it works
#' only when \code{method="historical"} and \code{portfolio_method="single"}.
#' @author @author Xin Chen, \email{chenx26@uw.edu}
#' @seealso \code{\link{SharpeRatio.annualized}} \cr
#' \code{\link{InformationRatio}} \cr \code{\link{TrackingError}} \cr
#' \code{\link{ActivePremium}} \cr \code{\link{SortinoRatio}} \cr
#' \code{\link{VaR}} \cr \code{\link{ES}} \cr
#' @references Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio
#' Management},Fall 1994, 49-58.
#'
#' Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk
#' Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002,
#' v 5.
###keywords ts multivariate distribution models
#' @examples
#'
#' data(managers)
#' SharpeRatio.SE(managers[,1,drop=FALSE], Rf=.035/12, FUN="StdDev")
#' SharpeRatio.SE(managers[,1,drop=FALSE], Rf = managers[,10,drop=FALSE], FUN="StdDev")
#' SharpeRatio.SE(managers[,1:6], Rf=.035/12, FUN="StdDev")
#' SharpeRatio.SE(managers[,1:6], Rf = managers[,10,drop=FALSE], FUN="StdDev")
#'
#'
#'
#' data(edhec)
#' SharpeRatio.SE(edhec[, 6, drop = FALSE], FUN="VaR")
#' SharpeRatio.SE(edhec[, 6, drop = FALSE], Rf = .04/12, FUN="VaR")
#' SharpeRatio.SE(edhec[, 6, drop = FALSE], Rf = .04/12, FUN="VaR" , method="gaussian")
#' SharpeRatio.SE(edhec[, 6, drop = FALSE], FUN="ES")
#'
#' # and all the methods
#' SharpeRatio.SE(managers[,1:9], Rf = managers[,10,drop=FALSE])
#' SharpeRatio.SE(edhec,Rf = .04/12)
#'
#' # Test the standard error functionalities
#' \dontrun{
#' h2o.init()
#' (res=SharpeRatio.SE(edhec, FUN = "StdDev",
#'                    se.method = c("IFiid","IFcor","BOOTiid","BOOTcor")))
#' # h2o.shutdown(prompt=FALSE)
#' printSE(res, round.digit = 5)
#' }
#' @export
#' @rdname SharpeRatio.SE
SharpeRatio.SE <-
  function (R, Rf = 0, p = 0.95, FUN=c("StdDev", "VaR","ES"),
            weights=NULL, annualize = FALSE , ...,
            se.method = "none"){
    mySR = SharpeRatio(R = R, Rf = Rf, p = p, FUN = FUN,
                       weights = weights, annualize = annualize, ...)

    if(length(FUN)==1 & FUN=="StdDev" & is.null(weights) & annualize == FALSE & se.method!="none"){

      R = checkData(R)

      if(!is.null(dim(Rf)))
        Rf = checkData(Rf)

      res=list(SR=mySR)
      # for each of the method specified in se.method, compute the standard error
      for(mymethod in se.method){
        res[[mymethod]]=EstimatorSE(R, estimator.fun = "SR", se.method = mymethod, rf = Rf)
      }
      return(res)
    } else {
      return(mySR)
    }
  }
