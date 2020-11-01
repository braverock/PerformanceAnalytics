#' calculate Omega for a return series
#' 
#' Keating and Shadwick (2002) proposed Omega (referred to as Gamma in their
#' original paper) as a way to capture all of the higher moments of the returns
#' distribution.
#' 
#' Mathematically, Omega is: integral[L to b](1 - F(r))dr / integral[a to
#' L](F(r))dr
#' 
#' where the cumulative distribution F is defined on the interval (a,b). L is
#' the loss threshold that can be specified as zero, return from a benchmark
#' index, or an absolute rate of return - any specified level. When comparing
#' alternatives using Omega, L should be common.
#' 
#' Input data can be transformed prior to calculation, which may be useful for
#' introducing risk aversion.
#' 
#' This function returns a vector of Omega, useful for plotting.  The steeper,
#' the less risky.  Above it's mean, a steeply sloped Omega also implies a very
#' limited potential for further gain.
#' 
#' Omega has a value of 1 at the mean of the distribution.
#' 
#' Omega is sub-additive.  The ratio is dimensionless.
#' 
#' Kazemi, Schneeweis, and Gupta (2003), in "Omega as a Performance Measure"
#' show that Omega can be written as: Omega(L) = C(L)/P(L) where C(L) is
#' essentially the price of a European call option written on the investment
#' and P(L) is essentially the price of a European put option written on the
#' investment.  The maturity for both options is one period (e.g., one month)
#' and L is the strike price of both options.
#' 
#' The numerator and the denominator can be expressed as: exp(-Rf=0) * E[max(x
#' - L, 0)] exp(-Rf=0) * E[max(L - x, 0)] with exp(-Rf=0) calculating the
#' present values of the two, where rf is the per-period riskless rate.
#' 
#' The first three methods implemented here focus on that observation. The
#' first method takes the simplification described above.  The second uses the
#' Black-Scholes option pricing as implemented in fOptions.  The third uses the
#' binomial pricing model from fOptions.  The second and third methods are not
#' implemented here.
#' 
#' The fourth method, "interp", creates a linear interpolation of the cdf of
#' returns, calculates Omega as a vector, and finally interpolates a function
#' for Omega as a function of L.  This method requires library \code{Hmisc},
#' which can be found on CRAN.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param L L is the loss threshold that can be specified as zero, return from
#' a benchmark index, or an absolute rate of return - any specified level
#' @param method one of: simple, interp, binomial, blackscholes
#' @param output one of: point (in time), or full (distribution of Omega)
#' @param Rf risk free rate, as a single number
#' @param SE TRUE/FALSE whether to ouput the standard errors of the estimates of the risk measures, default FALSE.
#' @param SE.control Control parameters for the computation of standard errors. Should be done using the \code{\link{RPESE.control}} function.
#' @param \dots any other passthru parameters
#' @author Peter Carl
#' @seealso \code{\link[Hmisc]{Ecdf}}
#' @references Keating, J. and Shadwick, W.F. The Omega Function. working
#' paper. Finance Development Center, London. 2002. Kazemi, Schneeweis, and
#' Gupta. Omega as a Performance Measure. 2003.
###keywords ts multivariate distribution models
#' @examples
#' 
#'     data(edhec)
#'     Omega(edhec)
#'     Omega(edhec[,13],method="interp",output="point")
#'     Omega(edhec[,13],method="interp",output="full")
#' 
#' @export
Omega <-
function(R, L = 0, method = c("simple", "interp", "binomial", "blackscholes"), 
         output = c("point", "full"), Rf = 0, 
         SE=FALSE, SE.control=NULL,
         ...)
{ # @author Peter Carl

    # DESCRIPTION
    # Keating and Shadwick (2002) proposed Omega (referred to as Gamma in their
    # original paper) as a way to capture all of the higher moments of the
    # returns distribution.  Mathematically, Omega is:
    #   integral[L to b](1 - F(r))dr / integral[a to L](F(r))dr
    # where the cumulative distribution F is defined on the interval (a,b).
    # L is the loss threshold that can be specified as zero, return from a
    # benchmark index, or an absolute rate of return - any specified level.
    # When comparing alternatives using Omega, L should be common.  Input data
    # can be transformed prior to calculation, which may be useful for
    # introducing risk aversion.

    # This function returns a vector of Omega, useful for plotting.  The
    # steeper, the less risky.  Above it's mean, a steeply sloped Omega also
    # implies a very limited potential for further gain.

    # Omega has a value of 1 at the mean of the distribution.

    # Omega is sub-additive.  The ratio is dimensionless.

    # Kazemi, Schneeweis, and Gupta (2003), in "Omega as a Performance Measure"
    # shows that Omega can be written as:
    #   Omega(L) = C(L)/P(L)
    # where C(L) is essentially the price of a European call option written
    # on the investment and P(L) is essentially the price of a European put
    # option written on the investment.  The maturity for both options is
    # one period (e.g., one month) and L is the strike price of both options.

    # The numerator and the denominator can be expressed as:
    #   exp(-Rf) * E[max(x - L, 0)]
    #   exp(-Rf) * E[max(L - x, 0)]
    # with exp(-Rf) calculating the present values of the two, where Rf is
    # the per-period riskless rate.

    # The first three methods implemented here focus on that observation.
    # The first method takes the simplification described above.  The second
    # uses the Black-Scholes option pricing as implemented in fOptions.  The
    # third uses the binomial pricing model from fOptions.  The second and
    # third methods are not implemented here.

    # The fourth method, "interp", creates a linear interpolation of the cdf of
    # returns, calculates Omega as a vector, and finally interpolates a function
    # for Omega as a function of L.

    # FUNCTION
    method = method[1]
    output = output[1]
    
    # Checking input if SE = TRUE
    if(SE){
      SE.check <- TRUE
      if(!requireNamespace("RPESE", quietly = TRUE)){
        warning("Package \"RPESE\" needed for standard errors computation. Please install it.",
                call. = FALSE)
        SE <- FALSE
      }
      if(!(method %in% c("simple"))){
        warning("To return SEs, \"method\" must be \"simple\".",
                call. = FALSE)
        SE.check <- FALSE
      }
      if(!(output %in% c("point"))){
        warning("To return SEs, \"output\" must be \"point\".",
                call. = FALSE)
        SE.check <- FALSE
      }
    }
    
    # SE Computation
    if(SE){

      # Setting the control parameters
      if(is.null(SE.control))
        SE.control <- RPESE.control(estimator="OmegaRatio")
      
      # Computation of SE (optional)
      ses=list()
      # For each of the method specified in se.method, compute the standard error
      for(mymethod in SE.control$se.method){
        ses[[mymethod]]=RPESE::EstimatorSE(R, estimator.fun = "OmegaRatio", se.method = mymethod, 
                                           cleanOutliers=SE.control$cleanOutliers,
                                           fitting.method=SE.control$fitting.method,
                                           freq.include=SE.control$freq.include,
                                           freq.par=SE.control$freq.par,
                                           a=SE.control$a, b=SE.control$b,
                                           const = L,
                                           ...)
        ses[[mymethod]]=ses[[mymethod]]$se
      }
      ses <- t(data.frame(ses))
      # Removing SE output if inappropriate arguments
      if(!SE.check){
        ses.rownames <- rownames(ses)
        ses.colnames <- colnames(ses)
        ses <- matrix(NA, nrow=nrow(ses), ncol=ncol(ses))
        rownames(ses) <- ses.rownames
        colnames(ses) <- ses.colnames
      }
    }

    if (is.vector(R)) {
        x = na.omit(R)

        switch(method,
            simple = {
                numerator = exp(-Rf) * mean(pmax(x - L, 0))
                denominator = exp(-Rf) * mean(pmax(L - x, 0))
                omega = numerator/denominator
            },
            binomial = {
                warning("binomial method not yet implemented, using interp")
                method = "interp"
            },
            blackscholes = {
                warning("blackscholes method not yet implemented, using interp")
                method = "interp"
            },
            interp = {

                stopifnot(requireNamespace("Hmisc",quietly=TRUE))
                a = min(x)
                b = max(x)

                xcdf = Hmisc::Ecdf(x, pl=FALSE)
                f <- approxfun(xcdf$x,xcdf$y,method="linear",ties="ordered")

                if(output == "full") {
                    omega = as.matrix(cumsum(1-f(xcdf$x))/cumsum(f(xcdf$x)))
                    names(omega) = xcdf$x
                }
                else {
                # returns only the point value for L
                    # to get a point measure for omega, have to interpolate
                    omegafull = cumsum(1-f(xcdf$x))/cumsum(f(xcdf$x)) # ????????
                    g <- approxfun(xcdf$x,omegafull,method="linear",ties="ordered")
                    omega = g(L)
                }
            }
        ) # end method switch

        result = omega
    }
    else {
        if(length(Rf)>1) Rf<-mean(Rf)
        if(length(L)>1) L<-mean(L)
        
        R = checkData(R, method = "matrix", ... = ...)
        if(output=="full")
            R = R[,1,drop=FALSE] # constrain to one column
        result = apply(R, 2, Omega, L = L, method = method, output = output, Rf = Rf,
            ... = ...)
        if(output!="full") {
            dim(result) = c(1,NCOL(R))
            rownames(result) = paste("Omega (L = ", round(L*100,1),"%)", sep="")
        }
        colnames(result) = colnames(R)
        
        if(SE) # Check if SE computation
          return(rbind(result, ses)) else
            return(result)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2020 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
