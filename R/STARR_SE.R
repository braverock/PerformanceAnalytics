#' Wrapper function that computes STARR and the standard error of the estimate
#'
#' @param data data
#' @param \dots any other passthru parameters. This include two types of parameters.
#' The first type is parameters associated with the risk/performance measure, such as tail
#' probability for VaR and ES. The second type is the parameters associated with the metohd
#' used to compute the standard error. See \code{\link{SE.IF.iid}}, \code{\link{SE.IF.cor}},
#' \code{\link{SE.BOOT.iid}}, \code{\link{SE.BOOT.cor}} for details.
#' @param alpha tail probability
#' @param rf risk free rate
#' @param se.method a character string indicating which method should be used to compute
#' the standard error of the estimated standard deviation. One of \code{"none"} (default),
#' \code{"IFiid"}, \code{"IFcor"}, \code{"BOOTiid"}, \code{"BOOTcor"}. Currently, it works
#' only when \code{method="historical"} and \code{portfolio_method="single"}.
#'
#' @return a vector or a list depending on se.method
#' @export
#' @author Xin Chen, \email{chenx26@uw.edu}
#'

STARR.SE = function(data, ..., alpha = 0.1, rf = 0, se.method = "none"){
  data = checkData(data)
  mySTARR = apply(data, 2, STARR, alpha = alpha, rf = rf, ...)
  names(mySTARR) = colnames(data)
  if(se.method == "none" & length(se.method)==1){
    return(mySTARR)
  } else {
    res=list(STARR=mySTARR)
    # for each of the method specified in se.method, compute the standard error
    for(mymethod in se.method){
      res[[mymethod]]=EstimatorSE(data, estimator.fun = "STARR",
                                  se.method = mymethod, alpha=alpha,
                                  rf = rf, ...)
    }
    return(res)
  }
}
