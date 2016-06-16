

#' Standard Error of standard deviation for xts object
#'
#' @param x xts object
#' @param na.rm whether NAs should be omitted or not
#'
#' @return SE of SD for the xts object
#' @export
#'
#' @examples
#' SE.SD.iid.xts(rnorm(10))
SE.SD.iid.xts=function(x,na.rm=FALSE){
  if (is.vector(x) || is.null(ncol(x)) || ncol(x) == 1) {
    x <- as.numeric(x)
    if(na.rm) x <- na.omit(x)
    return(SE.SD.iid(x))
  }
  else {
    x <- coredata(x)
    if(na.rm) x <- na.omit(x)
    return(apply(x, 2, SE.SD.iid))
  }
}

#' Compute standard error of standard deviation estimate for iid data via bootstrapping
#'
#' @param x xts object of the data
#' @param na.rm remove NA or not
#'
#' @return SE of SD via bootstrapping
#' @export
#'
#' @examples
#' SE.SD.iid.boot.xts(rnorm(10))
SE.SD.iid.boot.xts=function(x,na.rm=FALSE){
  if (is.vector(x) || is.null(ncol(x)) || ncol(x) == 1) {
    x <- as.numeric(x)
    if(na.rm) x <- na.omit(x)
    return(SE.SD.iid.boot(x))
  }
  else {
    x <- coredata(x)
    if(na.rm) x <- na.omit(x)
    return(apply(x, 2, SE.SD.iid.boot))
  }
}

SE.SD.iid=function(data){
  data=data^2
  # mu=mean(data)
  # N=length(data)
  # variance=mean((data-mu)^2)
  return(sqrt(var(data)/2/sd(data)/length(data)))
}

SE.SD.iid.boot=function(data,boot.sim=100,boot.run=100){
  N=length(data)
  res=rep(0,boot.run)
  for(run.iter in 1:boot.run){
    res.sd=rep(0,boot.sim)
    for(sim.iter in 1:boot.sim){
      x=sample(data,N,replace = TRUE)
      res.sd[sim.iter]=sd(x)
    }
    res[run.iter]=sd(res.sd)
  }
  return(mean(res))
}

