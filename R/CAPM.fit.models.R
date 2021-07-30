#' Compare CAPM estimated using robust estimators with that estimated by OLS 
#'
#' This function provies a simple plug and play option for user to compare the
#' SFM estimates by lm and lmrobdetMM functions, using the fit.models framework.
#' This will allow for an easier comparison using charts and tables
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#'
#' @author Dhairya Jain
#' @examples 
#' 
#' # First we load the data
#'     data(managers)
#'     SFM.fit.models(managers[,1], 
#' 			managers[,8,drop=FALSE], 
#' 			Rf = managers[,10,drop=FALSE])
#'     SFM.fit.models(managers[,6], 
#' 			managers[,8,drop=FALSE], 
#' 			Rf=.035/12) 
#' 		 SFM.fit.models(managers[, "HAM2", drop=FALSE], 
#' 			managers[, "SP500 TR", drop=FALSE], 
#' 			Rf = managers[, "US 3m TR", drop=FALSE])
#' 		SFM.fit.models(managers[, "HAM2", drop=FALSE], 
#' 			managers[, "SP500 TR", drop=FALSE], 
#' 			Rf = 0.035/12)
#' 		SFM.fit.models(managers[, "HAM2", drop=FALSE], 
#' 			managers[, "SP500 TR", drop=FALSE], 
#' 			Rf = managers[,10,drop=FALSE])
#' 		
#' @rdname SFM.fit.models
#' @export SFM.fit.models CAPM.fit.models
SFM.fit.models <- CAPM.fit.models <-
function(Ra, Rb, Rf=0){
  # @author Dhairya Jain
  
  # DESCRIPTION:
  # Function to compare robust SFM estimation model vs OLS SFM estimates 
  
  # Inputs:
  # Ra: time series of returns for the asset being tested
  # Rb: time series of returns for the benchmark the asset is being gauged against
  # Rf: risk free rate in the same periodicity as the returns.  May be a time series
  #     of the same length as x and y.
  #
  # Output:
  # Graphs comparing models
  
  # FUNCTION:
  if (!require("fit.models")){
    
    c = readline(prompt = "You need to install package fit.models to use this function. Do you want to install it? (y|N): "); 
    if(c=="y" || c=="Y"){
      install.packages("fit.models");
      library("fit.models");
    }
    else{
      stop("Aborted")
    }
  }
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  if(!is.null(dim(Rf)))
    Rf = checkData(Rf)
  
  Ra.ncols = NCOL(Ra) 
  Rb.ncols = NCOL(Rb)
  
  # Step 2: tell fit.models lmrobdetMM can be compared to lm
  fmclass.add.class("lmfm", "lmrobdetMM")

  models <- SFM.coefficients(Ra, Rb, Rf=Rf, method="Both")
  LmFit  <- models$robust$model
  LmRobFit <- models$ordinary$model
  fmLSrob <- fit.models::fit.models(LmFit, LmRobFit)
  
  # class(fmLSrob)
  # names(fmLSrob)
  # coef(fmLSrob)
  # summary(fmLSrob)
  # help(plot.lmfm)
  # plot(fmLSrob) 
  # plot(fmLSrob, which.plots = 1)
  # plot(fmLSrob, which.plots = 2)
  # plot(fmLSrob, which.plots = c(1,2))
  plot(fmLSrob, which.plots = "ask")  # ask = 0 to exit
}