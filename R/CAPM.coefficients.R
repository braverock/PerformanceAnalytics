#' Calculate single factor model (CAPM) alpha and beta coefficients
#' 
#' The single factor model or CAPM is the beta of an asset to the variance 
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
#' @aliases CAPM.coefficients
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param \dots Parameters like method, family and other parameters like max.it or bb 
#' for lmrobdetMM regression.
#' @param method (Optional): string representing linear regression model, "LS" for Least Squares
#'                    and "Rob" for robust      
#' @param family (Optional): 
#'         If method == "Rob": 
#'           This is a string specifying the name of the family of loss function
#'           to be used (current valid options are "bisquare", "opt" and "mopt").
#'           Incomplete entries will be matched to the current valid options. 
#'           Defaults to "mopt".
#'         Else: the parameter is ignored
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
#'      SFM.coefficients(managers[,1,drop=FALSE], 
#' 			     managers[,8,drop=FALSE], method="Rob" 
#' 			     family="bisquare")
#'      SFM.coefficients(managers[,1:6,drop=FALSE], 
#' 			     managers[,8:7,drop=FALSE])
#'      SFM.coefficients(managers[,1,drop=FALSE], 
#' 			     managers[,8:7,drop=FALSE], method="Both") 
#' 			SFM.coefficients(managers[,1:9,drop=FALSE], 
#' 			     managers[,8:10,drop=FALSE]
#' 			     Rf = managers[,10,drop=FALSE],
#' 			     method="Both")
#'      SFM.coefficients(managers[,1,drop=FALSE], 
#' 			     managers[,8,drop=FALSE], 
#' 			     Rf = managers[,10,drop=FALSE],
#' 			     method="Rob", family="mopt")
#' 			SFM.coefficients(managers[,1:3], 
#' 			     managers[,8:7,drop=FALSE], 
#' 			     Rf=.035/12, method="Rob", 
#' 			     family="opt", bb=0.25, 
#' 			     max.it=200)
#'      SFM.coefficients(managers[,1:3], 
#' 			     managers[,8:7,drop=FALSE], 
#' 			     Rf=.035/12, method="Rob", 
#' 			     family="bisquare", bb=0.25, 
#' 			     max.it=200)
#'      SFM.coefficients(managers[,1], 
#' 			     managers[,8,drop=FALSE], 
#' 			     Rf=.035/12, method="Both", 
#' 			     family="opt") 
#' 		 
#'   	  
#' @rdname SFM.coefficients
#' @export SFM.coefficients CAPM.coefficients
SFM.coefficients <- CAPM.coefficients <- 
function(Ra, Rb, subset=TRUE, Rf=0, ...)
{# @author Peter Carl, Dhairya Jain
  
  # DESCRIPTION:
  # This is a wrapper for calculating a SFM coefficients.
  
  # Inputs:
  # Ra: vector of returns for the asset being tested
  # Rb: vector of returns for the benchmark the asset is being gauged against
  # subset: a logical vector
  # Rf: risk free rate in the same periodicity as the returns.  May be a vector
  #     of the same length as x and y.
  #   , method="LS", family="mopt"
  # method (Optional): string representing linear regression model, "LS" for Least Squares
  #                    and "Rob" for robust      
  # family (Optional): 
  #         If method == "Rob": 
  #           This is a string specifying the name of the family of loss function
  #           to be used (current valid options are "bisquare", "opt" and "mopt").
  #           Incomplete entries will be matched to the current valid options. 
  #           Defaults to "mopt".
  #         Else: the parameter is ignored
  
  # Output:
  # 
  
  # FUNCTION:
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  if(!is.null(dim(Rf)))
    Rf = checkData(Rf)
  
  Ra.ncols = NCOL(Ra) 
  Rb.ncols = NCOL(Rb)
  
  xRa = Return.excess(Ra, Rf)
  xRb = Return.excess(Rb, Rf)
  
  pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
  
  result.all = apply(pairs, 1, FUN = function(n, xRa, xRb, subset, ...)
    .coefficients(xRa[,n[1]], xRb[,n[2]], subset, ...), 
    xRa = xRa, xRb = xRb, subset = subset, ...)
  
  if(length(result.all) ==1)
    return(result.all[[1]])
  else {
    dim(result.all) = c(Ra.ncols, Rb.ncols)
    colnames(result.all) = paste("Intercept, Beta, Model:", colnames(Rb))
    rownames(result.all) = colnames(Ra)
    return(t(result.all))
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
#' @param ... 
#' @param method Which linear model to use for SFM regression
#' @param family If method is "Rob", then this is a string specifying the 
#' name of the family of loss function to be used (current valid options are 
#' "bisquare", "opt" and "mopt"). Incomplete entries will be matched to the 
#' current valid options. Defaults to "mopt".
#' @author Dhairya Jain
.coefficients <- 
  function(xRa, xRb, subset, ..., method ="LS", family="mopt"){
    if (!require("RobStatTM")){
      c = readline(prompt = "You need to install package RobStatTM to use this function. Do you want to install it? (y|N): "); 
      if(c=="y" || c=="Y"){
        install.packages("RobStatTM");
        library("RobStatTM");
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
    colnames(merged) <- c("xRa", "xRb", "subset")
    merged$subset <- as.logical(merged$subset)
    switch(method,
           LS = {
             model.lm = lm(xRa ~ xRb, data=merged, subset=subset)
             return (list(intercept=coef(model.lm)[[1]],
                          beta=coef(model.lm)[[2]],
                          model= model.lm))
           },
           Rob = {
             model.rob.lm = lmrobdetMM(xRa ~ xRb, data=merged, 
                                       subset=subset, 
                                       control = lmrobdet.control(family=family, ...))
             return (list(intercept=coef(model.rob.lm)[[1]],
                          beta=coef(model.rob.lm)[[2]],
                          model= model.rob.lm))
           },
           Both = {
             model.rob.lm = lmrobdetMM(xRa ~ xRb, data=merged,
                                       subset=subset,
                                       control = lmrobdet.control(family=family))
             model.lm = lm(xRa ~ xRb, data=merged, subset=subset)
             return (list(robust=(list(intercept=coef(model.rob.lm)[[1]],
                                       beta=coef(model.rob.lm)[[2]],
                                       model=model.rob.lm
             )
             ),
             ordinary=(list(intercept=coef(model.lm)[[1]],
                            beta=coef(model.lm)[[2]],
                            model=model.lm
             )
             )
             )
             )
           },
           stop("SFM.coefficients:- Please enter a valid value (\"LS\",\"Rob\",\"Both\") for the parameter \"method\"")
    )
  }