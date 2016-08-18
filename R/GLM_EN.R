#' Compute the periodogram as defined in H&W 1981
#'
#' @param data Vector of data
#' @param max.freq Maximum frequency to be computed
#' @author Xin Chen, \email{chenx26@uw.edu}
#'
#' @return list of frequencies and corresponding periodograms
#' @export
#'
#' @examples
#' myperiodogram(rnorm(10))
myperiodogram=function(data,max.freq=0.5){
  ## data.fft=myfft(data) This is very slow
  data.fft=fft(data)
  N=length(data)
  #   tmp=1:N
  #   inset=tmp[(1:N)<floor(N/2)]
  return(list(spec=Mod(data.fft[1:floor(N/2)])^2/N,freq=((0:(floor(N/2)-1))/N)))
}



###### function to compute the standard error of sample mean of the data, to get asymptotic SE, multiply the SE by sqrt(length(data))
###### Using GLM with L1 regularization to fit a polynomial from the periodogram
###### then the intercept of the fitted polynomial is the spectral density p(0) at frequency 0
###### the variance of of the sample mean is p(0)/length(data)
###### and the asymptotic variance of the sample mean is p(0)
###### K is the proportion of frequencies to keep, 1 means keep all

#' Compute the variance of sample mean of the data
#'
#' Using GLM with L1 regularization to fit a polynomial from the periodogram then the intercept of the fitted polynomial is the spectral density p(0) at frequency 0
#'
#' @param data Vector of data
#' @param d Maximum order of the polynomial
#' @param alpha Weight of regularization. alpha=1 LASSO, alpha=0 Ridge
#' @param keep Percentage of periodograms to use for regression
#'
#' @return variance of the sample mean. To get the asymptotic variance, multiply by length(data)
#' @export
#'
#' @examples
#' require(h2o)
#' h2o.init()
#' SE.GLM.LASSO(rnorm(10))
#' # h2o.shutdown(prompt=FALSE)
SE.GLM.LASSO=function(data,d=5,alpha=1,keep=1){
  h2o.no_progress()
  N=length(data)
  # Step 1: compute the periodograms
  my.periodogram=myperiodogram(data)
  my.freq=my.periodogram$freq
  my.periodogram=my.periodogram$spec

  # remove values of frequency 0 as it does not contain information about the variance
  my.freq=my.freq[-1]
  my.periodogram=my.periodogram[-1]

  # implement cut-off
  nfreq=length(my.freq)
  my.freq=my.freq[1:floor(nfreq*keep)]
  my.periodogram=my.periodogram[1:floor(nfreq*keep)]

  # Step 2: use GLM with L1 regularization to fit polynomial

  # create 1, x, x^2, ..., x^d
  x.mat=rep(1,length(my.freq))
  for(col.iter in 1:d){
    x.mat=cbind(x.mat,my.freq^col.iter)
  }
  x.df=as.data.frame(x.mat)
  colnames(x.df)=paste0("V",0:d)
  x.df=cbind(my.periodogram,x.df)
  x.h2o.df=as.h2o(x.df)

  # fit GLM with L1 regularization

  my.glm.lasso=h2o.glm(x=colnames(x.h2o.df)[-1],y=colnames(x.h2o.df)[1],
                       training_frame = x.h2o.df, family = "gamma",
                       link="log",lambda_search = TRUE,alpha=alpha,
                       ignore_const_cols = FALSE)
  my.glm.lasso@model$coefficients

  # predict with new data V0=1 and all other variables=0
  newx.mat=matrix(c(1,rep(0,d)),nrow=1)
  newx.df=as.data.frame(newx.mat)
  colnames(newx.df)=paste0("V",0:d)
  newx.h2o.df=as.h2o(newx.df)
  p0.hat=h2o.predict(my.glm.lasso,newx.h2o.df)[1,1]


  # Step 4: return the estimated standard error
  return(sqrt(p0.hat/N))
}
