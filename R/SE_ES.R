#' Wrapper function to compute the standard error for ES estimator
#'
#' @param data xts object
#' @param ... extra arugements to be passed to lower level functions, see details
#' @param se.method a character string indicating which method should be used to compute
#' the standard error of the estimated standard deviation. One of \code{"none"} (default),
#' \code{"IFiid"}, \code{"IFcor"}, \code{"BOOTiid"}, \code{"BOOTcor"}. Currely, only \code{"IFiid"}
#' is implemented.
#'
#' @return SE of ES
#' @author Xin Chen
#' @export
#'
#' @examples
#' data(edhec)
#' SE.ES(edhec,se.method="IFiid")
SE.ES = function(data,...,se.method=c("none","IFiid","IFcor","BOOTiid","BOOTcor")){
  se.method=se.method[1]
  res = switch(se.method,
               none = NULL,
               IFiid = SE.ES.iid.xts(data,...),
               IFcor = SE.ES.cor.xts(data,...),
               BOOTiid = SE.ES.boot.iid.xts(data,...),
               BOOTcor = SE.ES.boot.cor.xts(data,...)
  )
  return(res)
}

#' Wrapper function to compute the standard error(s) of the ES for an xts object
#' using IFiid formula
#'
#' The operation is performed column wise.
#'
#' @param x the xts object
#' @param alpha tail probability
#'
#' @return standard error(s) of the xts object
#' @export
#'
#' @examples
#' data(edhec)
#' SE.ES.iid.xts(edhec)
SE.ES.iid.xts = function(x,alpha=0.05){
  if (is.vector(x) || is.null(ncol(x)) || ncol(x) == 1) {
    x <- as.numeric(x)
    #    if(na.rm) x <- na.omit(x)
    return(SE.ES.iid(x,alpha=alpha))
  }
  else {
    x <- coredata(x)
    #    if(na.rm) x <- na.omit(x)
    return(apply(x, 2, SE.ES.iid,alpha=alpha))
  }
}

#' Compute standard error of expected shortfall
#'
#' @param data vector of data
#' @param alpha tail probability
#'
#' @return SE of ES
#' @export
#'
#' @examples
#' SE.ES.iid(rnorm(10))
SE.ES.iid = function(data,alpha=0.05){
  N=length(data)
  VaR.hat=-quantile(data,alpha)
  ES.hat=-mean(data[data<=-VaR.hat])
  V=mean(data^2*(data<=-VaR.hat))/alpha^2
  +(1/alpha-1)*VaR.hat^2
  +(2-2/alpha)*ES.hat*VaR.hat
  -ES.hat^2
  return(sqrt(V/N))
}

#' Wrapper function to compute the standard error(s) of the ES for an xts object
#' using GLM-EN method
#'
#' The operation is performed column wise.
#'
#' @param x the xts object
#' @param alpha tail probability
#'
#' @return standard error(s) of the xts object
#' @export

SE.ES.cor.xts = function(x,alpha=0.05){
  if (is.vector(x) || is.null(ncol(x)) || ncol(x) == 1) {
    x <- as.numeric(x)
    #    if(na.rm) x <- na.omit(x)
    return(SE.ES.cor(x,alpha=alpha))
  }
  else {
    x <- coredata(x)
    #    if(na.rm) x <- na.omit(x)
    return(apply(x, 2, SE.ES.cor,alpha=alpha))
  }
}

#' Compute the standard error of ES estimator for correlated data vector using
#' GLM-EN method
#'
#' @param x the vector for the data
#' @param d the maximum order of the polynomial
#' @param alpha.lasso the weight for lasso vs elastic net
#' @param keep the portion of peridograms to use to fit the polynomial
#' @param alpha the tail probability of ES estimator
#'
#' @return the SE of the ES estimtor for the data
#' @export

SE.ES.cor = function(x, d = 5, alpha.lasso = 0.5, keep = 1, alpha = 0.05){
  N=length(x)
  data.IF = ES.IF(x, alpha = alpha)
  tmp = SE.GLM.LASSO(data.IF, d = d, alpha = alpha.lasso, keep = keep)
  return(sqrt(tmp))
}


#' Wrapper function to compute the standard error(s) of the ES for an xts object
#' using iid bootstrapping
#'
#' The operation is performed column wise.
#'
#' @param x the xts object
#' @param alpha tail probability
#' @param ... other parameters
#'
#' @return standard error(s) of the xts object
#' @export
#'
#' @examples
#' data(edhec)
#' SE.ES.boot.iid.xts(edhec)
SE.ES.boot.iid.xts = function(x,...,alpha=0.05){
  if (is.vector(x) || is.null(ncol(x)) || ncol(x) == 1) {
    x <- as.numeric(x)
    #    if(na.rm) x <- na.omit(x)
    return(SE.ES.boot.iid(x,...,alpha=alpha))
  }
  else {
    x <- coredata(x)
    #    if(na.rm) x <- na.omit(x)
    return(apply(x, 2, SE.ES.boot.iid,...=...,alpha=alpha))
  }
}

#' Compute standard error of ES via bootstrapping for iid data
#'
#' @param data vector of data
#' @param ... parameters passed from upper calls
#' @param alpha tail probability
#'
#' @return SE of ES
#' @export
#'
#' @examples
#' SE.ES.boot.iid(rnorm(10))
SE.ES.boot.iid = function(data, ...,alpha=0.05){
  args=list(...)
  arg.names=names(args)
  boot.sim=100
  boot.run=100
  # check if the parameters are specified by the function call
  if(("boot.sim" %in% arg.names) & (!is.null(args["boot.sim"]))) boot.sim=args["boot.sim"]
  if(("boot.run" %in% arg.names) & (!is.null(args["boot.run"]))) boot.run=args["boot.run"]
  N=length(data)
  res=rep(0,boot.run)
  # compute boot.run standard errors
  for(run.iter in 1:boot.run){
    res.sd=rep(0,boot.sim)
    # each standard error is computed from boot.sim ESs
    for(sim.iter in 1:boot.sim){
      x=sample(data,N,replace = TRUE)
      res.sd[sim.iter]=ES.hist(x,alpha)
    }
    res[run.iter]=sd(res.sd)
  }
  # return the ES of the boot.run standard errors
  return(mean(res))
}

#' Wrapper function to compute the standard error(s) of the ES for an xts object
#' using time series bootstrapping
#'
#' The operation is performed column wise.
#'
#' @param x the xts object
#' @param alpha tail probability
#' @param ... other parameters
#'
#' @return standard error(s) of the xts object
#' @export
#'
#' @examples
#' data(edhec)
#' SE.ES.boot.cor.xts(edhec)
SE.ES.boot.cor.xts = function(x,...,alpha=0.05){
  if (is.vector(x) || is.null(ncol(x)) || ncol(x) == 1) {
    x <- as.numeric(x)
    #    if(na.rm) x <- na.omit(x)
    return(SE.ES.boot.cor(x,...,alpha=alpha))
  }
  else {
    x <- coredata(x)
    #    if(na.rm) x <- na.omit(x)
    return(apply(x, 2, SE.ES.boot.cor,...=...,alpha=alpha))
  }
}

#' Compute standard error of ES via bootstrapping for data (possibily serially correlated)
#'
#' @param data vector of data
#' @param ... parameters passed from upper calls
#' @param alpha tail probability
#' @param segment.length the length of the fixed block for bootstrapping
#'
#' @return SE of ES
#' @export
#'
#' @examples
#' SE.ES.boot.cor(rnorm(100), alpha=0.1)
SE.ES.boot.cor = function(data, ...,alpha=0.05,segment.length=length(data)/5){
  args=list(...)
  arg.names=names(args)
  boot.sim=100
  boot.run=100
  N=length(data)
  # check if the parameters are specified by the function call
  if(("boot.sim" %in% arg.names) & (!is.null(args["boot.sim"]))) boot.sim=args["boot.sim"]
  if(("boot.run" %in% arg.names) & (!is.null(args["boot.run"]))) boot.run=args["boot.run"]
  res=rep(0,boot.run)
  # compute boot.run standard errors
  for(run.iter in 1:boot.run){
    boot.res=tsboot(data, ES.hist, R = boot.sim, sim = "fixed", l = segment.length, alpha = alpha)
    res[run.iter]=sd(boot.res$t)
  }
  # return the ES of the boot.run standard errors
  return(mean(res))
}

#' Compute sample Expected Shortfall
#'
#' @param data Vector of data
#' @param alpha Tail Probability
#'
#' @return sample ES
#' @export
#'
#' @examples
#' ES.hist(rnorm(10))
ES.hist=function(data,alpha=0.1){
  return(-mean(data[data<=quantile(data,alpha)]))
}


