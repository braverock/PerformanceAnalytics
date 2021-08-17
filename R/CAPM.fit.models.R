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
#' @param which.plots specify the plot numbers that you want to see. Default
#'        option is that program will ask you to choose a plot
#' @author Dhairya Jain
#' @examples 
#' 
#' # First we load the data
#'     data(managers)
#'       SFM.fit.models(managers[,1], 
#' 		   	    managers[,8], 
#' 		   	    Rf = managers[,10])
#'       SFM.fit.models(managers[,6], 
#' 		   	    managers[,8], 
#' 		   	    Rf=.035/12) 
#' 		   SFM.fit.models(managers[, "HAM2"], 
#' 		   	    managers[, "SP500 TR"], 
#' 		   	    Rf = managers[, "US 3m TR"])
#' 		   SFM.fit.models(managers[, "HAM2"], 
#' 		   	    managers[, "SP500 TR"], 
#' 		   	    Rf = 0.035/12)
#' 		   SFM.fit.models(managers[, "HAM2"], 
#' 		   	    managers[, "SP500 TR"], 
#' 		   	    Rf = managers[,10])
#'       SFM.fit.models(managers[,6], 
#' 			    managers[,8], 
#' 			    Rf=.035/12, 
#' 			    family = "mopt", 
#' 			    which.plots = c(1,2)) 
#' 		   SFM.fit.models(managers[, "HAM2"], 
#' 			    managers[, "SP500 TR"], 
#' 			    Rf = managers[, "US 3m TR"],
#' 			    family = "opt",
#' 			    which.plots = c(1,2,6,7))
#' 		   SFM.fit.models(managers[, "HAM2"], 
#' 			    managers[, "SP500 TR"], 
#' 			    Rf = 0.035/12, which.plots = NULL)
#' @rdname SFM.fit.models
#' @export SFM.fit.models CAPM.fit.models
SFM.fit.models <- CAPM.fit.models <-
function(Ra, Rb, Rf=0, family = "mopt", which.plots = NULL){
  # @author Dhairya Jain
  
  # DESCRIPTION:
  # Function to compare robust SFM estimation model vs OLS SFM estimates 
  
  # Inputs:
  # Ra: time series of returns for the asset being tested
  # Rb: time series of returns for the benchmark the asset is being gauged against
  # Rf: risk free rate in the same periodicity as the returns.  May be a time series
  #     of the same length as x and y.
  # family (Optional): 
  #         If method == "Rob": 
  #           This is a string specifying the name of the family of loss function
  #           to be used (current valid options are "bisquare", "opt" and "mopt").
  #           Incomplete entries will be matched to the current valid options. 
  #           Defaults to "mopt".
  #         Else: the parameter is ignored
  # which.plots: specify the plot numbers that you want to see. Default
  #              option is that program will ask you to choose a plot
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
  fam <- family
  models <- SFM.coefficients(Ra, Rb, Rf=Rf, family=fam, method="Both")
  LmFit  <- models$robust$model
  LmRobFit <- models$ordinary$model
  #if (fmclass %in% names(e$fmreg)) {
  #  message(fmclass, " is already registered in the fit.models registry")
  #  return(invisible())
  #}
  #print(names(e$fmreg))
  fmLSrob <- fit.models::fit.models(LmFit, LmRobFit)
  
  if(is.null(which.plots)){
    plot(fmLSrob, which.plots = "ask")  # ask = 0 to exit
  }
  else {
    plot(fmLSrob, which.plots = which.plots)
  }
}