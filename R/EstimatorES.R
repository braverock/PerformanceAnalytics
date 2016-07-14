#' Wrapper function for computing the standard error of risk/performance measure estimators
#'
#' @param data xts object of the data
#' @param ... extra arugements to be passed to lower level functions, see details
#' @param estimator.fun a character string indicating which risk/performancem measure's standard error
#' should be computed. One of \code{"Mean"} (default), \code{"SD"}, \code{"VaR"}, \code{"ES"},
#' \code{"SR"}, \code{"SoR"}, \code{"STARR"}.
#' @param se.method a character string indicating which method should be used to compute
#' the standard error of the estimated standard deviation. One of \code{"none"} (default),
#' \code{"IFiid"}, \code{"IFcor"}, \code{"BOOTiid"}, \code{"BOOTcor"}. Currely, only \code{"IFiid"}
#' is implemented.
#'
#' @return the standard error of the specified risk/performance measure using the specified method
#' @export
#' @author Xin Chen
#'
#' @examples
#' data(edhec)
#' EstimatorSE(edhec,estimator.fun="SD",se.method="IFiid")

EstimatorSE = function(data, ...,
                   estimator.fun = c("Mean","SD","VaR","ES","SR","SoR","STARR"),
                   se.method = c("none","IFiid","IFcor","BOOTiid","BOOTcor")){
  estimator.fun = estimator.fun[1]
  se.method = se.method[1]
  res = switch(estimator.fun,
               Mean = SE.Mean(data,...,se.method = se.method),
               SD = SE.SD(data,...,se.method = se.method),
               VaR = SE.VaR(data,...,se.method = se.method),
               ES = SE.ES(data,...,se.method = se.method),
               SR = SE.SR(data,...,se.method = se.method),
               SoR = SE.SoR(data,...,se.method = se.method),
               STARR = SE.STARR(data,...,se.method = se.method)
  )
  return(res)
}





