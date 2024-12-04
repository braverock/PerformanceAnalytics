###############################################################################
# $Id$
###############################################################################





#' calculate various Value at Risk (VaR) measures
#' 
#' Calculates Value-at-Risk(VaR) for univariate, component, and marginal cases
#' using a variety of analytical methods.
#' 
#' 
#' @aliases VaR VaR.CornishFisher
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param p confidence level for calculation, default p=.95
#' @param method one of "modified","gaussian","historical", "kernel", see
#' Details.
#' @param clean method for data cleaning through \code{\link{Return.clean}}.
#' Current options are "none", "boudt", "geltner", or "locScaleRob".
#' @param portfolio_method one of "single","component","marginal" defining
#' whether to do univariate, component, or marginal calc, see Details.
#' @param weights portfolio weighting vector, default NULL, see Details
#' @param mu If univariate, mu is the mean of the series. Otherwise mu is the
#' vector of means of the return series, default NULL, see Details
#' @param sigma If univariate, sigma is the variance of the series. Otherwise
#' sigma is the covariance matrix of the return series, default NULL, see
#' Details
#' @param m3 If univariate, m3 is the skewness of the series. Otherwise m3 is
#' the coskewness matrix (or vector with unique coskewness values) of the 
#' returns series, default NULL, see Details
#' @param m4 If univariate, m4 is the excess kurtosis of the series. Otherwise
#' m4 is the cokurtosis matrix (or vector with unique cokurtosis values) of the 
#' return series, default NULL, see Details
#' @param invert TRUE/FALSE whether to invert the VaR measure.  see Details.
#' @param SE TRUE/FALSE whether to ouput the standard errors of the estimates of the risk measures, default FALSE.
#' @param SE.control Control parameters for the computation of standard errors. Should be done using the \code{\link{RPESE.control}} function.
#' @param \dots any other passthru parameters
#' @note The option to \code{invert} the VaR measure should appease both
#' academics and practitioners.  The mathematical definition of VaR as the
#' negative value of a quantile will (usually) produce a positive number.
#' Practitioners will argue that VaR denotes a loss, and should be internally
#' consistent with the quantile (a negative number).  For tables and charts,
#' different preferences may apply for clarity and compactness.  As such, we
#' provide the option, and set the default to TRUE to keep the return
#' consistent with prior versions of PerformanceAnalytics, but make no value
#' judgment on which approach is preferable.
#' 
#' The prototype of the univariate Cornish Fisher VaR function was completed by
#' Prof. Diethelm Wuertz.  All corrections to the calculation and error
#' handling are the fault of Brian Peterson.
#' 
#' @section Background :
#' 
#' This function provides several estimation methods for
#' the Value at Risk (typically written as VaR) of a return series and the
#' Component VaR of a portfolio. Take care to capitalize VaR in the commonly
#' accepted manner, to avoid confusion with var (variance) and VAR (vector
#' auto-regression).  VaR is an industry standard for measuring downside risk.
#' For a return series, VaR is defined as the high quantile (e.g. ~a 95% or 99%
#' quantile) of the negative value of the returns. This quantile needs to be
#' estimated.  With a sufficiently large data set, you may choose to utilize
#' the empirical quantile calculated using \code{\link{quantile}}.  More
#' efficient estimates of VaR are obtained if a (correct) assumption is made on
#' the return distribution, such as the normal distribution.  If your return
#' series is skewed and/or has excess kurtosis, Cornish-Fisher estimates of VaR
#' can be more appropriate.  For the VaR of a portfolio, it is also of interest
#' to decompose total portfolio VaR into the risk contributions of each of the
#' portfolio components.  For the above mentioned VaR estimators, such a
#' decomposition is possible in a financially meaningful way.
#' 
#' @section Univariate VaR estimation methods :
#' 
#' The VaR at a probability level \eqn{p} (e.g. 95\%) is the \eqn{p}-quantile of
#' the negative returns, or equivalently, is the negative value of the
#' \eqn{c=1-p} quantile of the returns. In a set of returns for which
#' sufficently long history exists, the per-period Value at Risk is simply the
#' quantile of the period negative returns :
#'   
#'   \deqn{VaR=q_{.99}}{VaR=quantile(-R,p)}
#' 
#' where  \eqn{q_{.99}} is the 99\% empirical quantile of the negative return series.
#' 
#' This method is also sometimes called \dQuote{historical VaR}, as it is by
#' definition \emph{ex post} analysis of the return distribution, and may be
#' accessed with \code{method="historical"}.
#' 
#' When you don't have a sufficiently long set of returns to use non-parametric
#' or historical VaR, or wish to more closely model an ideal distribution, it is
#' common to us a parmetric estimate based on the distribution. J.P. Morgan's
#' RiskMetrics parametric mean-VaR was published in 1994 and this methodology
#' for estimating parametric mean-VaR has become what most literature generally
#' refers to as \dQuote{VaR} and what we have implemented as \code{\link{VaR}}.
#' See \cite{Return to RiskMetrics: Evolution of a
#' Standard}\url{https://www.msci.com/documents/10199/dbb975aa-5dc2-4441-aa2d-ae34ab5f0945}.
#' 
#' 
#' Parametric mean-VaR does a better job of accounting for the tails of the
#' distribution by more precisely estimating shape of the distribution tails of
#' the risk quantile. The most common estimate is a normal (or Gaussian)
#' distribution  \eqn{R\sim N(\mu,\sigma)} for the return series. In this case,
#' estimation of VaR requires the mean return  \eqn{\bar{R}}, the return
#' distribution and the variance of the returns  \eqn{\sigma}. In the most
#' common case, parametric VaR is thus calculated by
#' 
#' \deqn{\sigma=variance(R)}{sigma=var(R)}
#' 
#' \deqn{VaR=-\bar{R} -  \sqrt{\sigma} \cdot z_{c} }{VaR= -mean(R) - sqrt(sigma)*qnorm(c)}
#' 
#' where  \eqn{z_{c}} is the  \eqn{c}-quantile of the standard normal distribution. Represented in \R by \code{qnorm(c)},
#' and may be accessed with \code{method="gaussian"}.
#' 
#' Other forms of parametric mean-VaR estimation utilize a different
#' distribution for the distribution of losses to better account for the
#' possible fat-tailed nature of downside risk. The now-archived package
#' \code{VaR} contained methods for simulating and estimating lognormal and
#' generalized Pareto distributions to overcome some of the problems with
#' nonparametric or parametric mean-VaR calculations on a limited sample size or
#' on potentially fat-tailed distributions. There was also a
#' VaR.backtest function to apply simulation methods to create a more robust
#' estimate of the potential distribution of losses. Less commonly a covariance
#' matrix of multiple risk factors may be applied. This functionality should 
#' probably be 
#' 
#' The limitations of mean Value-at-Risk are well covered in the literature.
#' The limitations of traditional mean-VaR are all related to the use of a
#' symetrical distribution function. Use of simulations, resampling, or Pareto
#' distributions all help in making a more accurate prediction, but they are
#' still flawed for assets with significantly non-normal (skewed or kurtotic)
#' distributions. Zangari (1996) and Favre and Galeano(2002) provide a modified
#' VaR calculation that takes the higher moments of non-normal distributions
#' (skewness, kurtosis) into account through the use of a Cornish Fisher
#' expansion, and collapses to standard (traditional) mean-VaR if the return
#' stream follows a standard distribution. This measure is now widely cited and
#' used in the literature, and is usually referred to as \dQuote{Modified VaR}
#' or \dQuote{Modified Cornish-Fisher VaR}. They arrive at their modified VaR
#' calculation in the following manner:
#'   
#' \deqn{z_{cf}=z_{c}+\frac{(z_{c}^{2}-1)S}{6}+\frac{(z_{c}^{3}-3z_{c})K}{24}-\frac{(2z_{c}^{3}-5z_{c})S^{2}}{36}}{
#'     z_cf=z_c+[(z_c^2-1)S]/6+[(z_c^3-3z_c)K]/24-[(2z_c^3-5z_c)S^2]/36}
#' 
#' \deqn{Cornish-Fisher VaR =-\bar{R} - \sqrt(\sigma) \cdot z_{cf}}{VaR= -mean(R) - sqrt(sigma)*z_cf}
#' 
#' where \eqn{S} is the skewness of \eqn{R} and \eqn{K} is the excess kurtosis of \eqn{R}.
#' 
#' Cornish-Fisher VaR collapses to traditional mean-VaR when returns are
#' normally distributed. As such, the \code{\link{VaR}} and \code{\link{VaR}}
#' functions are wrappers for the \code{VaR} function. The Cornish-Fisher
#' expansion also naturally encompasses much of the variability in returns that
#' could be uncovered by more computationally intensive techniques such as
#' resampling or Monte-Carlo simulation.  This is the default method for the
#' \code{VaR} function, and may be accessed by setting \code{method="modified"}.
#' 
#' 
#' Favre and Galeano also utilize modified VaR in a modified Sharpe Ratio as the
#' return/risk measure for their portfolio optimization analysis, see
#' \code{\link{SharpeRatio.modified}} for more information.
#' 
#' @section Component VaR :
#' 
#' By setting \code{portfolio_method="component"} you may calculate the risk
#' contribution of each element of the portfolio.  The return from the function
#' in this case will be a list with three components: the univariate portfolio
#' VaR, the scalar contribution of each component to the portfolio VaR (these
#' will sum to the portfolio VaR), and a percentage risk contribution (which
#' will sum to 100\%).
#' 
#' Both the numerical and percentage component contributions to VaR may contain
#' both positive and negative contributions.  A negative contribution to
#' Component VaR indicates a portfolio risk diversifier.  Increasing the
#' position weight will reduce overall portoflio VaR.
#' 
#' If a weighting vector is not passed in via \code{weights}, the function will
#' assume an equal weighted (neutral) portfolio.
#' 
#' Multiple risk decomposition approaches have been suggested in the literature. 
#' A naive approach is to set the risk contribution equal to the stand-alone risk.
#' This approach is overly simplistic and neglects important diversification
#' effects of the units being exposed differently to the underlying risk
#' factors. An alternative approach is to measure the VaR contribution as the
#' weight of the position in the portfolio times the partial derivative of the
#' portfolio VaR with respect to the component weight. \deqn{C_i \mbox{VaR} =
#' w_i \frac{ \partial \mbox{VaR} }{\partial w_i}.}{C[i]VaR =
#' w[i]*(dVaR/dw[i]).} Because the portfolio VaR is linear in position size, we
#' have that by Euler's theorem the portfolio VaR is the sum of these risk
#' contributions. Gourieroux (2000) shows that for VaR, this mathematical
#' decomposition of portfolio risk has a financial meaning. It equals the
#' negative value of the asset's expected contribution to the portfolio return
#' when the portfolio return equals the negative portfolio VaR:
#' 
#' \deqn{C_i \mbox{VaR} = = -E\left[ w_i r_{i} | r_{p} = - \mbox{VaR}\right]}{C[i]VaR = -E( w[i]r[i]|rp=-VaR ) }
#' 
#' For the decomposition of Gaussian VaR, the estimated mean and covariance
#' matrix are needed. For the decomposition of modified VaR, also estimates of
#' the coskewness and cokurtosis matrices are needed. If \eqn{r} denotes the
#' \eqn{Nx1} return vector and \eqn{mu} is the mean vector, then the \eqn{N
#' \times N^2} co-skewness matrix is \deqn{ m3 = E\left[ (r - \mu)(r - \mu)'
#' \otimes (r - \mu)'\right]}{m3 = E[ (r - mu)(r - mu)' \%x\%  (r - \mu)']} The
#' \eqn{N \times N^3} co-kurtosis matrix is
#' 
#' \deqn{ m_{4} =
#'   E\left[ (r - \mu)(r - \mu)' \otimes (r - \mu)'\otimes (r - \mu)'
#'   \right] }{E[ (r - \mu)(r - \mu)' \%x\% (r - \mu)'\%x\% (r - \mu)']}
#'   
#' where \eqn{\otimes}{\%x\%} stands for the Kronecker product. The matrices can
#' be estimated through the functions \code{skewness.MM} and \code{kurtosis.MM}.
#' More efficient estimators have been proposed by Martellini and Ziemann (2007)
#' and will be implemented in the future.
#' 
#' As discussed among others in Cont, Deguest and Scandolo (2007), it is
#' important that the estimation of the VaR measure is robust to single
#' outliers. This is especially the case for  modified VaR and its
#' decomposition, since they use higher order moments. By default, the portfolio
#' moments are estimated by their sample counterparts. If \code{clean="boudt"}
#' then the \eqn{1-p} most extreme observations are winsorized if they are
#' detected as being outliers. For more information, see Boudt, Peterson and
#' Croux (2008) and \code{\link{Return.clean}}.  If your data consist of returns
#' for highly illiquid assets, then \code{clean="geltner"} may be more
#' appropriate to reduce distortion caused by autocorrelation, see
#' \code{\link{Return.Geltner}} for details.
#' 
#' Epperlein and Smillie (2006) introduced a non-parametric kernel estimator for 
#' component risk contributions, which is available via \code{method="kernel"} 
#' and \code{portfolio_method="component"}.
#' 
#' @section Marginal VaR :
#' 
#' Different papers call this different things.  In the Denton and Jayaraman
#' paper referenced here, this calculation is called Incremental VaR. We have
#' chosen the more common usage of calling this difference in VaR's in
#' portfolios without the instrument and with the instrument as the
#' \dQuote{difference at the Margin}, thus the name Marginal VaR. This is
#' incredibly confusing, and hasn't been resolved in the literature at this
#' time.
#' 
#' Simon Keel and David Ardia (2009) attempt to reconcile some of the
#' definitional issues and address some of the shortcomings of this measure in
#' their working paper titled \dQuote{Generalized Marginal Risk}. Hopefully 
#' their improved Marginal Risk measures may be included here in the future. 
#'
#'
#' @author Brian G. Peterson and Kris Boudt
#' @seealso \code{\link{SharpeRatio.modified}} \cr
#' \code{\link{chart.VaRSensitivity}} \cr
#' \code{\link{Return.clean}} 
#' @references Boudt, Kris, Peterson, Brian, and Christophe Croux. 2008.
#' Estimation and decomposition of downside risk for portfolios with non-normal
#' returns. 2008. The Journal of Risk, vol. 11, 79-103.
#' 
#' Cont, Rama, Deguest, Romain and Giacomo Scandolo. Robustness and sensitivity
#' analysis of risk measurement procedures. Financial Engineering Report No.
#' 2007-06, Columbia University Center for Financial Engineering.
#' 
#' Denton M. and Jayaraman, J.D. Incremental, Marginal, and Component VaR.
#' Sunguard. 2004.
#' 
#' Epperlein, E., Smillie, A. Cracking VaR with kernels. RISK, 2006, vol.  19,
#' 70-74.
#' 
#' Gourieroux, Christian, Laurent, Jean-Paul and Olivier Scaillet.  Sensitivity
#' analysis of value at risk. Journal of Empirical Finance, 2000, Vol. 7,
#' 225-245.
#' 
#' Keel, Simon and Ardia, David. Generalized marginal risk. Aeris CAPITAL
#' discussion paper.
#' 
#' Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk
#' Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002,
#' v 5.
#' 
#' Martellini, L. and Ziemann, V., 2010. Improved estimates of higher-order 
#' comoments and implications for portfolio selection. Review of Financial 
#' Studies, 23(4):1467-1502.
#' 
#' Return to RiskMetrics: Evolution of a Standard
#' \url{https://www.msci.com/documents/10199/dbb975aa-5dc2-4441-aa2d-ae34ab5f0945}
#' 
#' Zangari, Peter. A VaR Methodology for Portfolios that include Options. 1996.
#' RiskMetrics Monitor, First Quarter, 4-12.
#' 
#' Rockafellar, Terry and Uryasev, Stanislav. Optimization of Conditional VaR.
#' The Journal of Risk, 2000, vol. 2, 21-41.
#' 
#' Dowd, Kevin. Measuring Market Risk, John Wiley and Sons, 2010.
#' 
#' Jorian, Phillippe. Value at Risk, the new benchmark for managing financial risk.
#' 3rd Edition, McGraw Hill, 2006.
#' 
#' Hallerback, John. "Decomposing Portfolio Value-at-Risk: A General Analysis",
#' 2003. The Journal of Risk vol 5/2.
#' 
#' Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and 
#'    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization",
#'    Bank of Japan.
#'      
###keywords ts multivariate distribution models
#' @examples
#' 
#' if(!( Sys.info()[['sysname']]=="Windows") ){
#' # if on Windows, cut and paste this example
#' 
#'     data(edhec)
#' 
#'     # first do normal VaR calc
#'     VaR(edhec, p=.95, method="historical")
#' 
#'     # now use Gaussian
#'     VaR(edhec, p=.95, method="gaussian")
#' 
#'     # now use modified Cornish Fisher calc to take non-normal distribution into account
#'     VaR(edhec, p=.95, method="modified")
#' 
#'     # now use p=.99
#'     VaR(edhec, p=.99)
#'     # or the equivalent alpha=.01
#'     VaR(edhec, p=.01)
#' 
#'     # now with outliers squished
#'     VaR(edhec, clean="boudt")
#' 
#'     # add Component VaR for the equal weighted portfolio
#'     VaR(edhec, clean="boudt", portfolio_method="component")
#' 
#' } # end Windows check
#' 
#' @export
VaR <-
  function (R=NULL , p=0.95, ..., method=c("modified","gaussian","historical", "kernel"), 
            clean=c("none","boudt","geltner","locScaleRob"),  
            portfolio_method=c("single","component","marginal"), 
            weights=NULL, mu=NULL, sigma=NULL, m3=NULL, m4=NULL, invert=TRUE,
            SE=FALSE, SE.control=NULL)
  { # @author Brian G. Peterson
    
    # Descripion:
    
    # wrapper for univariate and multivariate VaR functions.
    
    # Setup:
    #if(exists(modified)({if( modified == TRUE) { method="modified" }}
    #if(method == TRUE or is.null(method) ) { method="modified" }
    clean = clean[1]
    method = method[1]
    portfolio_method = portfolio_method[1]
    
    # Checking input if SE = TRUE
    if(SE){
      SE.check <- TRUE
      if(!requireNamespace("RPESE", quietly = TRUE)){
        warning("Package \"RPESE\" needed for standard errors computation. Please install it.",
                call. = FALSE)
        SE <- FALSE
      }
      if(!(clean %in% c("none", "locScaleRob"))){
        warning("To return SEs, \"clean\" must be one of \"locScaleRob\" or \"none\".",
                call. = FALSE)
        SE.check <- FALSE
      }
      if(!(portfolio_method %in% c("single"))){
        warning("To return SEs, \"portfolio_method\" must be \"single\".",
                call. = FALSE)
        SE.check <- FALSE
      }
      if(!(method %in% c("historical"))){
        warning("To return SEs, \"method\" must be \"historical\".",
                call. = FALSE)
        SE.check <- FALSE
      }
      if(invert){
        warning("To return SEs, \"invert\" must be FALSE.",
                call. = FALSE)
        SE.check <- FALSE
      }
    }
    
    # SE Computation
    if(SE){
      
      # Setting the control parameters
      if(is.null(SE.control))
        SE.control <- RPESE.control(estimator="VaR")
      
      # Computation of SE (optional)
      ses=list()
      # For each of the method specified in se.method, compute the standard error
      for(mymethod in SE.control$se.method){
        ses[[mymethod]]=RPESE::EstimatorSE(R, estimator.fun = "VaR", se.method = mymethod, 
                                           cleanOutliers=SE.control$cleanOutliers,
                                           fitting.method=SE.control$fitting.method,
                                           freq.include=SE.control$freq.include,
                                           freq.par=SE.control$freq.par,
                                           a=SE.control$a, b=SE.control$b,
                                           alpha = p,
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
    
    if (is.null(weights) & portfolio_method != "single"){
      message("no weights passed in, assuming equal weighted portfolio")
      weights=t(rep(1/dim(R)[[2]], dim(R)[[2]]))
    }
    if(!is.null(R)){
      R <- checkData(R, method="xts", ...)
      columns=colnames(R)
      if (!is.null(weights) & portfolio_method != "single") {
        if ( length(weights) != ncol(R)) {
          stop("number of items in weights not equal to number of columns in R")
        }
      }
      # weights = checkData(weights, method="matrix", ...) #is this necessary?
      # TODO check for date overlap with R and weights
      if(clean!="none" & is.null(mu)){ # the assumption here is that if you've passed in any moments, we'll leave R alone
        R = as.matrix(Return.clean(R, method=clean))
      }
      if(portfolio_method != "single"){
        # get the moments ready
        if (is.null(mu)) { mu =  apply(R,2,'mean' ) }
        if (is.null(sigma)) { sigma = cov(R) }
        if(method=="modified"){
          if (is.null(m3)) {m3 = M3.MM(R,as.mat=FALSE)}
          if (is.null(m4)) {m4 = M4.MM(R,as.mat=FALSE)}
        }
      } 
    } else { 
      #R is null, check for moments
      if(is.null(mu)) stop("Nothing to do! You must pass either R or the moments mu, sigma, etc.")
      if ( length(weights) != length(mu)) {
        stop("number of items in weights not equal to number of items in the mean vector")
      }
    }
    
    if (!is.null(R)){
    }
    
    switch(portfolio_method,
           single = {
             if(is.null(weights)){
               switch(method,
                      modified = { rVaR = VaR.CornishFisher(R=R,p=p) }, # mu=mu, sigma=sigma, skew=skew, exkurt=exkurt))},
                      gaussian = { rVaR = VaR.Gaussian(R=R,p=p) },
                      historical = { rVaR = -1* t(apply(R, 2, quantile, probs=1-p, na.rm=TRUE )) },
                      kernel = { stop("no kernel method defined for non-component VaR")}
               ) # end single switch calc
               # convert from vector to columns
               rVaR=as.matrix(rVaR)
               colnames(rVaR)=columns
             } else { # we have weights, so we should use the .MM calc
               weights=as.vector(weights)
               switch(method,
                      modified = { rVaR=mVaR.MM(w=weights, mu=mu, sigma=sigma, M3=m3 , M4=m4 , p=p) }, 
                      gaussian = { rVaR=GVaR.MM(w=weights, mu=mu, sigma=sigma, p=p) },
                      historical = { rVaR = as.matrix(VaR.historical(R=R,p=p)) * weights } # note that this is weighting the univariate calc by the weights
               ) # end multivariate method
             }
             columns<-ncol(rVaR)
             for(column in 1:columns) {
               tmp=rVaR[,column]
               if (!is.finite(tmp)) # skip reasonableness check if tmp is NA, NaN, +/-Inf, etc
                 next()
               if (eval(tmp < 0)) { #eval added previously to get around Sweave bitching
                 message(c("VaR calculation produces unreliable result (inverse risk) for column: ",column," : ",rVaR[,column]))
                 # set VaR to NA, since inverse risk is unreasonable
                 rVaR[,column] <- NA
               } else
                 if (eval(1 < tmp)) { #eval added previously to get around Sweave bitching
                   message(c("VaR calculation produces unreliable result (risk over 100%) for column: ",column," : ",rVaR[,column]))
                   # set VaR to 1, since greater than 100% is unreasonable
                   rVaR[,column] <- 1
                 }
             } # end reasonableness checks
             if(invert) rVaR <- -rVaR
             rownames(rVaR)<-"VaR"
             
             if(SE) # Check if SE computation
               return(rbind(rVaR,ses)) else
                 return(rVaR)
           }, # end single portfolio switch
           component = {
             # @todo need to add another loop here for subsetting, I think, when weights is a timeseries
             #if (mu=NULL or sigma=NULL) {
             #     pfolioret = Return.portfolio(R, weights, wealth.index = FALSE, contribution=FALSE, method = c("simple"))
             #}
             # for now, use as.vector
             weights=as.vector(weights)
             names(weights)<-colnames(R)
             
             switch(method,
                    modified = { return(VaR.CornishFisher.portfolio(p,weights,mu,sigma,m3,m4))},
                    gaussian = { return(VaR.Gaussian.portfolio(p,weights,mu,sigma)) },
                    historical = { return(VaR.historical.portfolio(R, p,weights)) },
                    kernel = { return(VaR.kernel.portfolio(R, p,weights)) }
             )
             
           }, # end component portfolio switch
           marginal = {
             return(VaR.Marginal(R,p,method,as.vector(weights)))
           }  # end marginal portfolio switch
    )
    
    } # end VaR wrapper function

###############################################################################
# R (https://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2020 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
