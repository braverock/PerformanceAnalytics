

#' Asymptotic Standard Error of standard deviation for xts object
#'
#' @param x xts object
#' @param na.rm whether NAs should be omitted or not
#'
#' @return Asymptotic SE of SD for the xts object
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

SE.SD.iid=function(data){
  data=data^2
  # mu=mean(data)
  # N=length(data)
  # variance=mean((data-mu)^2)
  return(sqrt(var(data)))
}
