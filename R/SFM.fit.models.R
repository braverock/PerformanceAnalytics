#' Compare SFM estimated using robust estimators with that estimated by OLS 
#'
#' This function provies a simple plug and play option for user to compare the
#' SFM estimates by lm and lmrobdetMM functions, using the fit.models framework.
#' This will allow for an easier comparison using charts and tables
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param family (Optional): 
#'         If method == "Robust": 
#'           This is a string specifying the name of the family of loss function
#'           to be used (current valid options are "bisquare", "opt" and "mopt").
#'           Incomplete entries will be matched to the current valid options. 
#'           Defaults to "mopt".
#'         Else: the parameter is ignored
#' @param which.plots specify the plot numbers that you want to see. Default
#'        option is that program will ask you to choose a plot
#' @param plots Boolean to output plots after the function. Defaults to TRUE
#' @author Dhairya Jain
#' @examples 
#' \dontrun{
#' # First we load the data
#'     data(managers)
#'     mgrs <- managers["2002/"]  # So that all have managers have complete history
#'     names(mgrs)[7:10] <- c("LSEQ","SP500","Bond10Yr","RF") # Short names for last 3
#'     
#'     # NOTE: Use plots=TRUE to be able to see plots
#'
#'     fitModels <- SFM.fit.models(mgrs$HAM1,mgrs$SP500,Rf = mgrs$RF, plots=FALSE)
#'     coef(fitModels)
#'     summary(fitModels)
#'     class(fitModels)
#'     
#'     SFM.fit.models(mgrs$HAM1, mgrs$SP500, Rf = mgrs$RF, plots=FALSE)
#'     
#'     SFM.fit.models(mgrs[,6], mgrs[,8], Rf=.035/12, plots=FALSE) 
#'     
#'     SFM.fit.models(mgrs$HAM6, mgrs$SP500, Rf=.035/12, family = "mopt", 
#'                    which.plots = c(1,2), plots=FALSE) 
#' 		 
#' 		 SFM.fit.models(mgrs$HAM2, mgrs$SP500, Rf = mgrs$RF, family = "opt", 
#' 		                plots=FALSE)
#' }
#' @rdname SFM.fit.models
#' @export SFM.fit.models
SFM.fit.models <- function(Ra, Rb, Rf=0, family = "mopt", 
                           which.plots = NULL, plots=TRUE){
  # @author Dhairya Jain
  
  # DESCRIPTION:
  # Function to compare robust SFM estimation model vs OLS SFM estimates 
  
  # Inputs:
  # Ra: time series of returns for the asset being tested
  # Rb: time series of returns for the benchmark the asset is being gauged against
  # Rf: risk free rate in the same periodicity as the returns.  May be a time series
  #     of the same length as x and y.
  # family (Optional): 
  #         If method == "Robust": 
  #           This is a string specifying the name of the family of loss function
  #           to be used (current valid options are "bisquare", "opt" and "mopt").
  #           Incomplete entries will be matched to the current valid options. 
  #           Defaults to "mopt".
  #         Else: the parameter is ignored
  # which.plots: specify the plot numbers that you want to see. Default
  #              option is that program will ask you to choose a plot
  # plots: Boolean to output plots after the function. Defaults to TRUE
  # Output:
  # Graphs comparing models & a fitted object is returned, If plots = TRUE is used
  # Only a fitted object is returned, If plots = FALSE is used 
  
  # FUNCTION:
  
  # Sort the dependencies
  if (!requireNamespace("fit.models", quietly=TRUE)){
    c = readline(prompt = "You need to install package fit.models to use this function. Do you want to install it? (y|N): "); 
    if(c=="y" || c=="Y"){
      # Install the package
      utils::install.packages("fit.models")
    }
    else{
      stop("Aborted");
    }
  }
  
  # Get the NCOL and colnames from Ra, and Rb
  Ra.ncols <- NCOL(Ra);
  Rb.ncols <- NCOL(Rb);
  Ra.colnames <- colnames(Ra);
  Rb.colnames <- colnames(Rb)
  
  # Get the excess returns of Ra, Rb over Rf
  xRa = Return.excess(Ra, Rf)
  xRb = Return.excess(Rb, Rf)
  
  # Get the models
  models <- getResults(xRa, xRb, Ra.ncols, Rb.ncols, family=family, method="Both", subset=T)[[1]]

  # Get the LS and Rob models
  RobFit  <- models$robust$model
  LSFit <- models$LS$model
  
  # Tell fit.models lmrobdetMM can be compared to lm
  fit.models::fmclass.add.class("lmfm", "lmrobdetMM", warn=FALSE)
  
  # Fit both the models
  fmLSrob <- fit.models::fit.models(LSFit, RobFit)
  
  # Plot the fit.models object if asked
  if (plots==TRUE){
    if(is.null(which.plots)){
      plot(fmLSrob, which.plots = "ask")  # ask = 0 to exit
    }
    else {
      plot(fmLSrob, which.plots = which.plots)
    }
    
    # Return the fitted object, but make it invisible
    return (invisible(fmLSrob));
  }
  
  else{
    # Return the fitted object to the user
    return(fmLSrob);
  }
}