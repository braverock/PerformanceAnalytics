#' clean returns in a time series to to provide more robust risk estimates
#' 
#' A function that provides access to multiple methods for cleaning outliers
#' from return data.
#' 
#' This is a wrapper for offering multiple data cleaning methods for data
#' objects containing returns.
#' 
#' The primary value of data cleaning lies in creating a more robust and stable
#' estimation of the distribution generating the large majority of the return
#' data. The increased robustness and stability of the estimated moments using
#' cleaned data should be used for portfolio construction. If an investor
#' wishes to have a more conservative risk estimate, cleaning may not be
#' indicated for risk monitoring.
#' 
#' In actual practice, it is probably best to back-test the results of both
#' cleaned and uncleaned series to see what works best when forecasting risk
#' with the particular combination of assets under consideration.
#' 
#' In this version, only one method is supported.  See
#' \code{\link{clean.boudt}} for more details.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param method one of "none", "boudt", which applies the function
#' \code{\link{clean.boudt}} or "geltner" which applies the function
#' \code{\link{Return.Geltner}}to R
#' @param alpha the percentage of outliers you want to clean
#' @param \dots additional parameters passed into the underlying cleaning
#' function
#' @author Peter Carl
#' @seealso \code{\link{clean.boudt}} \cr \code{\link{Return.Geltner}} \cr
#' @keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' head(Return.clean(managers[,1:4]),n=20)
#' chart.BarVaR(managers[,1,drop=FALSE], show.clean=TRUE, clean="boudt", lwd=2, methods="ModifiedVaR")
#' 
#' @export
Return.clean <-
function(R, method = c("none","boudt","geltner"), alpha=.01, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper for selecting the method by which return data is 'cleaned'

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns

    # Outputs:
    # A timeseries of the 'cleaned' series

    # FUNCTION:
    method = method[1]

    # Transform input data to a timeseries (xts) object
    orig = R
    R = checkData(R, method="xts")

    #result.zoo = zoo(NA, order.by=time(R))

    # Get dimensions and labels
    columns = ncol(R)
    columnnames = colnames(R)

    for(column in 1:columns) { # for each asset passed in as R
        #R.clean = zoo(NA, order.by=time(R))

        switch(method,
            none = {
		R.clean = R[,column]
	    },
	    boudt = {
                R.clean = clean.boudt(na.omit(R[ , column, drop=FALSE]),alpha=alpha,...)[[1]]
            },
	    geltner = {
		R.clean = Return.Geltner(R[,column])
	    }
        )

        if(column == 1) {
            result = R.clean
        }
        else {
            result = cbind(result, R.clean)
        }
    }

    # RESULTS:
    result=reclass(result,match.to=orig)
    return(result)
}





#' clean extreme observations in a time series to to provide more robust risk
#' estimates
#' 
#' Robustly clean a time series to reduce the magnitude, but not the number or
#' direction, of observations that exceed the \eqn{1-\alpha\%} risk threshold.
#' 
#' Many risk measures are calculated by using the first two (four) moments of
#' the asset or portfolio return distribution. Portfolio moments are extremely
#' sensitive to data spikes, and this sensitivity is only exacerbated in a
#' multivariate context. For this reason, it seems appropriate to consider
#' estimates of the multivariate moments that are robust to return observations
#' that deviate extremely from the Gaussian distribution.
#' 
#' There are two main approaches in defining robust alternatives to estimate
#' the multivariate moments by their sample means (see e.g. Maronna[2006]). One
#' approach is to consider a more robust estimator than the sample means.
#' Another one is to first clean (in a robust way) the data and then take the
#' sample means and moments of the cleaned data.
#' 
#' Our cleaning method follows the second approach. It is designed in such a
#' way that, if we want to estimate downside risk with loss probability
#' \eqn{\alpha}{alpha}, it will never clean observations that belong to the
#' \eqn{1-\alpha}{(1-alpha)} least extreme observations. Suppose we have an
#' \eqn{n}-dimensional vector time series of length \eqn{T}: \eqn{r_1,...,r_T}.
#' We clean this time series in three steps.
#' 
#' \enumerate{ \item \emph{ Ranking the observations in function of their
#' extremeness. }Denote \eqn{\mu} and \eqn{\Sigma} the mean and covariance
#' matrix of the bulk of the data and let \eqn{\lfloor \cdot \rfloor}{floor()}
#' be the operator that takes the integer part of its argument. As a measure of
#' the extremeness of the return observation \eqn{r_t}, we use its squared
#' Mahalanobis distance \eqn{ d^2_t = (r_t-\mu)'\Sigma^{-1}(r_t-\mu)}.  We
#' follow Rousseeuw(1985) by estimating \eqn{\mu} and \eqn{\Sigma} as the mean
#' vector and covariance matrix (corrected to ensure consistency) of the subset
#' of size \eqn{\lfloor (1-\alpha)T\rfloor}{floor((1-\alpha)T)} for which the
#' determinant of the covariance matrix of the elements in that subset is the
#' smallest. These estimates will be robust against the \eqn{\alpha} most
#' extreme returns. Let \eqn{d^2_{(1)},...,d^2_{(T)}} be the ordered sequence
#' of the estimated squared Mahalanobis distances such that \eqn{d^2_{(i)}\leq
#' d^2_{(i+1)}}.
#' 
#' \item \emph{Outlier identification.} Return observations are qualified as
#' outliers if their estimated squared Mahalanobis distance \eqn{d^2_t} is
#' greater than the empirical \eqn{1-\alpha} quantile \eqn{d^2_{(\lfloor
#' (1-\alpha)T \rfloor)}}{floor((1-\alpha)T)} and exceeds a very extreme
#' quantile of the Chi squared distribution function with \eqn{n} degrees of
#' freedom, which is the distribution function of \eqn{d^2_t} when the returns
#' are normally distributed. In this application we take the 99.9\% quantile,
#' denoted \eqn{\chi ^2_{n,0.999}}.
#' 
#' \item \emph{Data cleaning. } Similarly to Khan(2007) we only clean the
#' returns that are identified as outliers in step 2 
#' by replacing these returns \eqn{r_t} with 
#' \deqn{r_t\sqrt{\frac{\max(d^2_{(\lfloor(1-\alpha)T)\rfloor},\chi^2_{n,0.999})}{d^2_t}}}{r_t * sqrt(max(d^2_floor((1-\alpha)T),\chi^2_{n,0.999})/d^2_t)}
#' The cleaned
#' return vector has the same orientation as the original return vector, but
#' its magnitude is smaller. Khan(2007) calls this procedure of limiting the
#' value of \eqn{d^2_t} to a quantile of the \eqn{\chi^2_n} distribution,
#' ``multivariate Winsorization'.
#' 
#' }
#' 
#' Note that the primary value of data cleaning lies in creating a more robust
#' and stable estimation of the distribution generating the large majority of
#' the return data. The increased robustness and stability of the estimated
#' moments utilizing cleaned data should be used for portfolio construction. If
#' a portfolio manager wishes to have a more conservative risk estimate,
#' cleaning may not be indicated for risk monitoring. It is also important to
#' note that the robust method proposed here does not remove data from the
#' series, but only decreases the magnitude of the extreme events. It may also
#' be appropriate in practice to use a cleaning threshold somewhat outside the
#' VaR threshold that the manager wishes to consider. In actual practice, it is
#' probably best to back-test the results of both cleaned and uncleaned series
#' to see what works best with the particular combination of assets under
#' consideration.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param alpha probability to filter at 1-alpha, defaults to .01 (99\%)
#' @param trim where to set the "extremeness" of the Mahalanobis distance
#' @return cleaned data matrix
#' @note This function and much of this text was originally written for Boudt,
#' et. al, 2008
#' @author Kris Boudt, Brian G. Peterson
#' @seealso \code{\link{Return.clean}}
#' @references Boudt, K., Peterson, B. G., Croux, C., 2008. Estimation and
#' Decomposition of Downside Risk for Portfolios with Non-Normal Returns.
#' Journal of Risk, forthcoming.
#' 
#' Khan, J. A., S. Van Aelst, and R. H. Zamar (2007). Robust linear model
#' selection based on least angle regression. Journal of the American
#' Statistical Association 102.
#' 
#' Maronna, R. A., D. R. Martin, and V. J. Yohai (2006). Robust Statistics:
#' Theory and Methods. Wiley.
#' 
#' Rousseeuw, P. J. (1985). Multivariate estimation with high breakdown point.
#' In W. Grossmann, G. Pflug, I. Vincze, and W. Wertz (Eds.), Mathematical
#' Statistics and Its Applications, Volume B, pp. 283?297. Dordrecht-Reidel.
#' @keywords ts multivariate distribution models
#' @export
clean.boudt <-
function(R, alpha=.01 , trim=1e-3)
{# @author Kris Boudt, Brian Peterson

   # set up by loading robustbase library
   stopifnot("package:robustbase" %in% search() || require("robustbase",quietly=TRUE))

   # Function used to bound the effect of the most extreme returns on the downside
   # risk prediction.

   R=checkData(R,method="zoo") # modified to create a zoo object in the cleaneddata slot of the list

   T=dim(R)[1]; date=c(1:T)
   N=dim(R)[2];
   MCD = robustbase::covMcd(as.matrix(R),alpha=1-alpha)
   mu = as.matrix(MCD$raw.center) #no reweighting
   sigma = MCD$raw.cov
   invSigma = solve(sigma);
   vd2t = c();
   cleaneddata = R
   outlierdate = c()

   # 1. Sort the data in function of their extremeness
   # Extremeness is proxied by the robustly estimated squared Mahalanbobis distance

   for(t in c(1:T) )
   {
      d2t = as.matrix(R[t,]-mu)%*%invSigma%*%t(as.matrix(R[t,]-mu));
      vd2t = c(vd2t,d2t);
   }
   out = sort(vd2t,index.return=TRUE)
   sortvd2t = out$x;
   sortt = out$ix;

   # 2. Outlier detection
   # empricical 1-alpha quantile

   empirical.threshold = sortvd2t[floor((1-alpha)*T)];

   # 2.1. Only the alpha most extreme observations can be qualified as outliers

   T.alpha = floor(T * (1-alpha))+1
   # print(c("empirical quantile=",vd2t[sortt[T.alpha-1]],"chi-squared quantile",qchisq(1-trim,N)))

   cleanedt=sortt[c(T.alpha:T)]

   # 2.2. multivariate winsorization (Khan et al, 2007) :
   # bound the MD of the most exteme observations to a quantile of the chi squared distribution, N d
   for(t in cleanedt ){
        if(vd2t[t]>qchisq(1-trim,N)){
              # print(c("Observation",as.character(date[t]),"is detected as outlier and cleaned") );
               cleaneddata[t,] = sqrt( max(empirical.threshold,qchisq(1-trim,N))/vd2t[t])*R[t,];
               outlierdate = c(outlierdate,date[t]) } }

   return(list(cleaneddata,outlierdate))

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
