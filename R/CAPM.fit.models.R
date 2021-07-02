#' Compare CAPM estimated using robust estimators with that estimated by OLS 
#'
#' This function provies a simple plug and play option for user to compare the
#' CAPM estimates by lm and lmrobdetMM functions, using the fit.models framework.
#' This will allow for an easier comparison using charts and tables
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#'
#' @author Dhairya Jain
#' @export CAPM.fit.models SFM.fit.models
#'
#' @examples 
#' 
#' # First we load the data
#'     data(managers)
#'     CAPM.fit.models(managers[,1,drop=FALSE], 
#' 			managers[,8,drop=FALSE], 
#' 			Rf=.035/12) 
#'     CAPM.fit.models(managers[,1,drop=FALSE], 
#' 			managers[,8,drop=FALSE], 
#' 			Rf = managers[,10,drop=FALSE])
#'     CAPM.fit.models(managers[,1:6], 
#' 			managers[,8,drop=FALSE], 
#' 			Rf=.035/12)
#'     CAPM.fit.models(managers[,1:6], 
#' 			managers[,8,drop=FALSE], 
#' 			Rf = managers[,10,drop=FALSE])
#'     CAPM.fit.models(managers[,1:6], 
#' 			managers[,8:7,drop=FALSE], 
#' 			Rf=.035/12) 
#'     CAPM.fit.models(managers[,1:6], 
#' 			managers[,8:7,drop=FALSE], 
#' 			Rf = managers[,10,drop=FALSE])
#'     
CAPM.fit.models <- SFM.fit.models <-
function(Ra, Rb, Rf=0){
  # FUNCTION:
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  if(!is.null(dim(Rf)))
    Rf = checkData(Rf)
  
  Ra.ncols = NCOL(Ra) 
  Rb.ncols = NCOL(Rb)
  
  # Step 2: tell fit.models lmrobdetMM can be compared to lm
  fmclass.add.class("lmfm", "lmrobdetMM")
  xRa = Return.excess(Ra, Rf)
  xRb = Return.excess(Rb, Rf)
  
  models <- CAPM.coefficients(xRa, xRb, method="Both")
  fmLSrob <- fit.models::fit.models(models$robust, models$ordinary)
  class(fmLSrob)
  names(fmLSrob)
  coef(fmLSrob)
  summary(fmLSrob)
  help(plot.lmfm)
  plot(fmLSrob) 
  plot(fmLSrob, which.plots = 1)
  plot(fmLSrob, which.plots = 2)
  plot(fmLSrob, which.plots = c(1,2))
  plot(fmLSrob, which.plots = "ask")  # ask = 0 to exit
}