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
#' @author Xin Chen, \email{chenx26@uw.edu}
#'
#' @examples
#' data(edhec)
#' EstimatorSE(edhec,estimator.fun="SD",se.method="IFiid")
#' h2o.init()
#' EstimatorSE(edhec,estimator.fun="ES",se.method="IFcor")
#' # h2o.shutdown(prompt=FALSE)
#' EstimatorSE(edhec,estimator.fun="ES",se.method="BOOTiid", nsim=100)
#' EstimatorSE(edhec,estimator.fun="ES",se.method="BOOTcor", nsim=100,
#' sim = "fixed", l = round(nrow(edhec)/5))

EstimatorSE = function(data, ...,
                   estimator.fun = c("Mean","SD","VaR","ES","SR","SoR","STARR"),
                   se.method = c("none","IFiid","IFcor","BOOTiid","BOOTcor")){
  estimator.fun = estimator.fun[1]
  se.method = se.method[1]
  myfun = switch(estimator.fun,
                 Mean = mean,
                 SD = sd,
                 VaR = VaR.hist,
                 ES = ES.hist,
                 SR = SR,
                 SoR = SoR.const,
                 STARR = STARR
  )
  myfun.IF = switch (estimator.fun,
                     Mean = mu.IF,
                     SD = SD.IF,
                     VaR = VaR.IF,
                     ES = ES.IF,
                     SR = SR.IF,
                     SoR = SoR.const.IF,
                     STARR = STARR.IF
  )

  res = switch (se.method,
    none = NULL,
    IFiid = SE.xts(data, SE.IF.iid, myfun, myfun.IF, ...),
    IFcor = SE.xts(data, SE.IF.cor, myfun, myfun.IF, ...),
    BOOTiid = SE.xts(data, SE.BOOT.iid, myfun, myfun.IF, ...),
    BOOTcor = SE.xts(data, SE.BOOT.cor, myfun, myfun.IF,...)
  )

  return(res)
}

#' compute the standard error for the xts object
#'
#' @param x the xts object of the data
#' @param se.fun the function used to compute the standard error
#' @param myfun the measure
#' @param myfun.IF the influence function of the measure
#' @param ... other parameters
#'
#' @return standard errors
#' @export
#' @author Xin Chen, \email{chenx26@uw.edu}
#'
#' @examples
#' data(edhec)
#' SE.xts(edhec, SE.IF.iid, sd, SD.IF)
SE.xts = function(x, se.fun, myfun, myfun.IF, ...){
  if (is.vector(x) || is.null(ncol(x)) || ncol(x) == 1) {
    x <- as.numeric(x)
    #    if(na.rm) x <- na.omit(x)
    return(se.fun(x = x, myfun = myfun, myfun.IF = myfun.IF, ...))
  }
  else {
    x <- coredata(x)
    #    if(na.rm) x <- na.omit(x)
    return(apply(x, 2, se.fun, myfun = myfun, myfun.IF = myfun.IF, ... ))
  }
}

#' Compute the standard error using influence function approach for a vector
#'
#' @param x vector of data
#' @param myfun.IF influence function of the measure
#' @param ... other parameters used to compute the influence function
#'
#' @return standard error of the measure
#' @export
#' @author Xin Chen, \email{chenx26@uw.edu}
#'
#' @examples
#' SE.IF.iid(rnorm(100), SD.IF)
SE.IF.iid = function(x, myfun.IF, ...){
  N=length(x)
  x.IF = myfun.IF(x, ...)
  x.IF.2 = x.IF^2
  tmp = mean(x.IF.2)
  return(sqrt(tmp/N))
}

#' Compute the standard error using GLM-EN approach for serially correlated data
#'
#' @param x the vector of data
#' @param myfun.IF the influene function of the measure
#' @param d maximum order of the polynomial
#' @param alpha.lasso weight for the elastic net
#' @param keep portion of frequencies to be used
#' @param ... other parameters
#'
#' @return the standard error of the measure
#' @export
#' @author Xin Chen, \email{chenx26@uw.edu}
#'

SE.IF.cor = function(x, myfun.IF, ..., d = 5, alpha.lasso = 0.5, keep = 1){
  data.IF = myfun.IF(x, ...)
  tmp = SE.GLM.LASSO(data.IF, d = d, alpha = alpha.lasso, keep = keep)
  return(tmp)
}

#' Compute the standard error of the measure by iid bootstrapping
#'
#' @param x vector of data
#' @param myfun measure
#' @param nsim number of replicates
#' @param ... other parameters
#' @param myfun.IF not used
#'
#' @return standard error
#' @export
#' @author Xin Chen, \email{chenx26@uw.edu}
#'
#' @examples
#' SE.BOOT.iid(x = rnorm(100), myfun = mean, nsim = 100)
SE.BOOT.iid = function(x, myfun, myfun.IF, ..., nsim = 100){
  res = boot(data = x, statistic = function(x,i,...) myfun(x[i],...), R = nsim, ... = ...)
  return(sd(res$t))
}

#' Compute the standard error of the measure by tsboot()
#'
#' @param x vector of data
#' @param myfun measure
#' @param nsim number of replicates
#' @param ... other parameters
#' @param myfun.IF not used
#' @param sim the type of simulation
#' @param l the length of the fixed block
#'
#' @return standard error
#' @export
#' @author Xin Chen, \email{chenx26@uw.edu}
#'
#' @examples
#' SE.BOOT.cor(x = rnorm(100), myfun = mean, nsim = 100)
SE.BOOT.cor = function(x, myfun, myfun.IF, ..., nsim = 100,
                       sim = "fixed", l = round(length(x)/5)){
  res = tsboot(tseries = x, statistic = function(x,...) myfun(x,...), R = nsim,
               sim = sim, l = l,...)
  return(sd(res$t))
}








