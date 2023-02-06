#' Calculate single factor model alpha and beta coefficients
#' 
#' The single factor model is the beta of an asset to the variance 
#' and covariance of an initial portfolio. Used to determine diversification potential.
#' "Alpha" purports to be a measure of a manager's skill by measuring the
#' portion of the managers returns that are not attributable to "Beta", or the
#' portion of performance attributable to a benchmark.
#' 
#' This function is designed to be used a a wrapper for SFM's regression models. 
#' Using this, one can easily fit different types of linear models 
#' like Ordinary Least Squares or Robust Estimators like Huber.
#' 
#' \deqn{\beta_{a,b}=\frac{CoV_{a,b}}{\sigma_{b}}=\frac{\sum((R_{a}-\bar{R_{a}})(R_{b}-\bar{R_{b}}))}{\sum(R_{b}-\bar{R_{b}})^{2}}}{beta
#' = cov(Ra,Rb)/var(R)}
#' 
#' Ruppert(2004) reports that this equation will give the estimated slope of
#' the linear regression of \eqn{R_{a}}{Ra} on \eqn{R_{b}}{Rb} and that this
#' slope can be used to determine the risk premium or excess expected return
#' (see Eq. 7.9 and 7.10, p. 230-231).
#'
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param subset a logical vector representing the set of observations to be used in regression
#' @param \dots Other parameters like max.it or bb specific to lmrobdetMM regression. Another
#' interesting parameter is round.by.alphas, and round.by.betas which can be set to round off the 
#' returned values, otherwise the values 
#' @param method (Optional): string representing linear regression model, "LS" for Least Squares
#'                    and "Robust" for robust. Defaults to "LS      
#' @param family (Optional): 
#'         If method == "Robust": 
#'           This is a string specifying the name of the family of loss function
#'           to be used (current valid options are "bisquare", "opt" and "mopt").
#'           Incomplete entries will be matched to the current valid options. 
#'           Defaults to "mopt".
#'         Else: the parameter is ignored
#' @param digits (Optional): Number of digits to round the results to. Defaults to 3.
#' @param benchmarkCols (Optional): Boolean to show the benchmarks as columns. Defaults to TRUE.
#' @param Model (Optional): Boolean to return the fitted model. Defaults to FALSE
#' @param warning (Optional): Boolean to show warnings or not. Defaults to TRUE.
#' 
#' @author Dhairya Jain
#' @seealso \code{\link{BetaCoVariance}} \code{\link{SFM.alpha}}
#' \code{\link{CAPM.utils}} \code{\link{SFM.beta}}
#' @references Sharpe, W.F. Capital Asset Prices: A theory of market
#' equilibrium under conditions of risk. \emph{Journal of finance}, vol 19,
#' 1964, 425-442. \cr Ruppert, David. \emph{Statistics and Finance, an
#' Introduction}. Springer. 2004. \cr Bacon, Carl. \emph{Practical portfolio
#' performance measurement and attribution}. Wiley. 2004. \cr
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#'      SFM.coefficients(managers[,1], managers[,8]) 
#'      SFM.coefficients(managers[,1:6], managers[,8:9], Rf = managers[,10])
#' 			SFM.coefficients(managers[,1:6], managers[,8:9], 
#' 			     Rf=.035/12, method="Robust", 
#' 			     family="mopt", bb=0.25, 
#' 			     max.it=200)
#' 			     
#'   	  
#' @rdname SFM.coefficients
#' @export SFM.coefficients
SFM.coefficients <- function(Ra, Rb, Rf=0, subset=TRUE, ..., method="Robust", 
                             family="mopt", digits=3, benchmarkCols=T, Model=F, 
                             warning=T){
  # @author  Dhairya Jain
  
  # DESCRIPTION:
  # This is a wrapper for calculating a SFM coefficients.
  
  # Inputs:
  # Ra: vector of returns for the asset being tested
  # Rb: vector of returns for the benchmark the asset is being gauged against
  # Rf: risk free rate in the same periodicity as the returns.  May be a vector
  #     of the same length as x and y.
  #   , method="LS", family="mopt"
  # subset: a logical vector
  # method (Optional): string representing linear regression model, "LS" for Least Squares
  #                    and "Robust" for robust. Defaults to "LS      
  # family (Optional): 
  #         If method == "Robust": 
  #           This is a string specifying the name of the family of loss function
  #           to be used (current valid options are "bisquare", "opt" and "mopt").
  #           Incomplete entries will be matched to the current valid options. 
  #           Defaults to "mopt".
  #         Else: the parameter is ignored
  # digits (Optional): Number of digits to round the results to. Defaults to 3.
  # benchmarkCols (Optional): Boolean to show the benchmarks as columns. Defaults to TRUE.
  # Model (Optional): Boolean to return the fitted model. Defaults to FALSE
  # warning (Optional): Boolean to show warnings or not. Defaults to TRUE.
  
  # Output:
  #         When Model==T, returns a list(intercept, beta, model)
  #                     Otherwise
  #         Returns a matrix of data.
  
  # FUNCTION:
  
  # Get the NCOL and colnames from Ra, and Rb
  Ra.ncols <- NCOL(Ra);
  Rb.ncols <- NCOL(Rb);
  Ra.colnames <- colnames(Ra);
  Rb.colnames <- colnames(Rb)
  
  # ret.Model == TRUE means internal usage. Hence, if not internal usage
  if (Model==F){
    if (warning && method=="Both"){
      warning("Using 'Both' while using SFM.Coefficients will lead to ill-formatted output");
    }
    betaTable = SFM.beta(Ra, Rb, Rf = Rf, subset = subset, ..., method = method, family = family, warning = F, digits=digits, benchmarkCols=benchmarkCols)
    alphaTable = SFM.alpha(Ra, Rb, Rf = Rf, subset = subset, ..., method = method, family = family, warning = F, digits=digits, benchmarkCols=benchmarkCols)
    if (benchmarkCols){
      ret = cbind(alphaTable, betaTable)
      if (Ra.ncols==1 && Rb.ncols==1 && method!="Both"){
        colnames(ret) <- c("Alpha", "Beta")
      }
    }
    else{
      ret = rbind(alphaTable, betaTable)
      if (Ra.ncols==1 && Rb.ncols==1 && method!="Both"){
        rownames(ret) <- c("Alpha", "Beta")
      }
    }
    return(round(ret,digits))
  }
  
  # Get the excess returns of Ra, Rb over Rf
  xRa = Return.excess(Ra, Rf)
  xRb = Return.excess(Rb, Rf)
  
  
  pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
  
  result.all = apply(pairs, 1, FUN = function(n, xRa, xRb, subset, method, family, ...)
    .coefficients(xRa[,n[1]], xRb[,n[2]], subset, method = method, family = family, ...), 
    xRa = xRa, xRb = xRb, subset = subset, method = method, family = family, ...)
  
  if(length(result.all) ==1)
    return(result.all[[1]])
  else {
    if(Model){
      dim(result.all) = c(Ra.ncols, Rb.ncols)
      colnames(result.all) = paste("Intercept, Beta, Model:", colnames(Rb))
      rownames(result.all) = colnames(Ra)
      return(t(result.all))
    }
    else{
      stop("Bad Input. Support for method=='Both' is supported only when both
           Ra and Rb are single column objects")
    }
  }
}

#' Wrapper for SFM's regression models.
#' 
#' This is intended to be called using SFM.coefficients function, and
#' not directly.
#'
#' @param xRa an xts, vector, matrix, data frame, timeSeries or zoo object of
#' excess asset returns
#' @param xRb excess return vector of the benchmark asset
#' @param subset a logical vector
#' @param \dots arguments passed to other methods
#' @param method Which linear model to use for SFM regression
#' @param family If method is "Rob", then this is a string specifying the 
#' name of the family of loss function to be used (current valid options are 
#' "bisquare", "opt" and "mopt"). Incomplete entries will be matched to the 
#' current valid options. Defaults to "mopt".
#' @author Dhairya Jain
.coefficients <- 
  function(xRa, xRb, subset, ..., method ="LS", family="mopt"){
    # @author  Dhairya Jain
    
    # DESCRIPTION:
    # This is the main function calculating the regression.
    
    # Inputs:
    # xRa: vector of excess returns for the asset being tested
    # xRb: vector of excess returns for the benchmark the asset is being gauged against
    # subset: a logical vector
    # method (Optional): string representing linear regression model, "LS" for Least Squares
    #                    and "Robust" for robust. Defaults to "LS      
    # family (Optional): 
    #         If method == "Robust": 
    #           This is a string specifying the name of the family of loss function
    #           to be used (current valid options are "bisquare", "opt" and "mopt").
    #           Incomplete entries will be matched to the current valid options. 
    #           Defaults to "mopt".
    #         Else: the parameter is ignored
    
    # Output:
    #         Returns a list(intercept, beta, model)
    
    # FUNCTION:
    
    if (!requireNamespace("RobStatTM", quietly=TRUE)){
      c = readline(prompt = "You need to install package RobStatTM to use this function. Do you want to install it? (y|N): "); 
      if(c=="y" || c=="Y"){
        utils::install.packages("RobStatTM")
      }
      else{
        stop("Aborted")
      }
    }
    # subset is assumed to be a logical vector
    if(missing(subset))
      subset <-TRUE
    # check columns
    if(NCOL(xRa)!= 1L || NCOL(xRb)!= 1L || NCOL(subset)!= 1L)
      stop("all arguments must have only one column")
    # merge, drop NA
    merged <- as.data.frame(na.omit(cbind(xRa, xRb, subset)))
    # return NA if no non-NA values
    if(NROW(merged)== 0)
      return (NA)
    # add column names and convert subset back to logical
    colnames(merged) <- c("Alpha", "Beta", "subset")
    merged$subset <- as.logical(merged$subset)
    Alpha <- xRa
    Beta <- xRb
    switch(method,
           LS = {
             model.lm = lm(Alpha ~ Beta, data=merged, subset=subset)
             return (list(intercept=coef(model.lm)[[1]],
                          beta=coef(model.lm)[[2]],
                          model= model.lm))
           },
           Robust = {
             model.rob.lm = RobStatTM::lmrobdetMM(Alpha ~ Beta, data=merged, 
                                       subset=subset, 
                                       control = RobStatTM::lmrobdet.control(family=family, ...))
             return (list(intercept=coef(model.rob.lm)[[1]],
                          beta=coef(model.rob.lm)[[2]],
                          model= model.rob.lm))
           },
           Both = {
             model.rob.lm = RobStatTM::lmrobdetMM(Alpha ~ Beta, data=merged,
                                       subset=subset,
                                       control = RobStatTM::lmrobdet.control(family=family))
             model.lm = lm(Alpha ~ Beta, data=merged, subset=subset)
             return (list(robust=(list(intercept=coef(model.rob.lm)[[1]],
                                       beta=coef(model.rob.lm)[[2]],
                                       model=model.rob.lm
             )
             ),
             LS=(list(intercept=coef(model.lm)[[1]],
                            beta=coef(model.lm)[[2]],
                            model=model.lm
             )
             )
             )
             )
           },
           stop("SFM.coefficients:- Please enter a valid value (\"LS\",\"Robust\",\"Both\") for the parameter \"method\"")
    )
  }