###############################################################################
# $Id$
###############################################################################





#' calculates Standard Deviation for univariate and multivariate series, also
#' calculates component contribution to standard deviation of a portfolio
#'
#' calculates Standard Deviation for univariate and multivariate series, also
#' calculates component contribution to standard deviation of a portfolio
#'
#' TODO add more details
#'
#' This wrapper function provides fast matrix calculations for univariate,
#' multivariate, and component contributions to Standard Deviation.
#'
#' It is likely that the only one that requires much description is the
#' component decomposition.  This provides a weighted decomposition of the
#' contribution each portfolio element makes to the univariate standard
#' deviation of the whole portfolio.
#'
#' Formally, this is the partial derivative of each univariate standard
#' deviation with respect to the weights.
#'
#' As with \code{\link{VaR}}, this contribution is presented in two forms, both
#' a scalar form that adds up to the univariate standard deviation of the
#' portfolio, and a percentage contribution, which adds up to 100%.  Note that
#' as with any contribution calculation, contribution can be negative.  This
#' indicates that the asset in question is a diversified to the overall
#' standard deviation of the portfolio, and increasing its weight in relation
#' to the rest of the portfolio would decrease the overall portfolio standard
#' deviation.
#'
#' @param R a vector, matrix, data frame, timeSeries or zoo object of asset
#' returns
#' @param \dots any other passthru parameters. This include two types of parameters.
#' The first type is parameters associated with the risk/performance measure, such as tail
#' probability for VaR and ES. The second type is the parameters associated with the metohd
#' used to compute the standard error. See \code{\link{SE.IF.iid}}, \code{\link{SE.IF.cor}},
#' \code{\link{SE.BOOT.iid}}, \code{\link{SE.BOOT.cor}} for details.
#' @param clean method for data cleaning through \code{\link{Return.clean}}.
#' Current options are "none", "boudt", or "geltner".
#' @param portfolio_method one of "single","component" defining whether to do
#' univariate/multivariate or component calc, see Details.
#' @param weights portfolio weighting vector, default NULL, see Details
#' @param mu If univariate, mu is the mean of the series. Otherwise mu is the
#' vector of means of the return series , default NULL, , see Details
#' @param se.method a character string indicating which method should be used to compute
#' the standard error of the estimated standard deviation. One of \code{"none"} (default),
#' \code{"IFiid"}, \code{"IFcor"}, \code{"BOOTiid"}, \code{"BOOTcor"}. Currently, it works
#' only when \code{method="historical"} and \code{portfolio_method="single"}.
#' @param sigma If univariate, sigma is the variance of the series. Otherwise
#' sigma is the covariance matrix of the return series , default NULL, see
#' Details
#' @param use an optional character string giving a method for computing
#' covariances in the presence of missing values.  This must be (an
#' abbreviation of) one of the strings \code{"everything"}, \code{"all.obs"},
#' \code{"complete.obs"}, \code{"na.or.complete"}, or
#' \code{"pairwise.complete.obs"}.
#' @param method a character string indicating which correlation coefficient
#' (or covariance) is to be computed.  One of \code{"pearson"} (default),
#' \code{"kendall"}, or \code{"spearman"}, can be abbreviated.
#' @author @author Xin Chen, \email{chenx26@uw.edu}
#' @seealso \code{\link{Return.clean}} \code{sd}
###keywords ts multivariate distribution models
#' @examples
#'
#'     data(edhec)
#'
#'     # first do normal StdDev calc
#'     StdDev.SE(edhec)
#'     # or the equivalent
#'     StdDev.SE(edhec, portfolio_method="single")
#'
#'     # now with outliers squished
#'     StdDev.SE(edhec, clean="boudt")
#'
#'     # add Component StdDev for the equal weighted portfolio
#'     StdDev.SE(edhec, clean="boudt", portfolio_method="component")
#'
#'     # next use more than one method at the same time
#'     h2o.init()
#'     (res=StdDev.SE(edhec, se.method = c("IFiid","IFcor","BOOTiid","BOOTcor")))
#'     h2o.shutdown(prompt=FALSE)
#'     printSE(res)
#'
#'
#' @export
StdDev.SE <- function (R , ..., clean=c("none","boudt","geltner"),
                       portfolio_method=c("single","component"), weights=NULL, mu=NULL, sigma=NULL,
                       use="everything", method=c("pearson", "kendall", "spearman"),
                       se.method="none"){
  myStdDev = StdDev(R , ..., clean=c("none","boudt","geltner"),
                    portfolio_method=c("single","component"), weights=NULL, mu=NULL, sigma=NULL,
                    use="everything", method=c("pearson", "kendall", "spearman"))
  if(portfolio_method == "single" & is.null(weights)){
    portfolio_method = portfolio_method[1]
    clean = clean[1]
    R <- checkData(R, method="xts", ...)
    columns=colnames(R)

    if(clean!="none"){
      R = as.matrix(Return.clean(R, method=clean))
    }
    if(se.method == "none" & length(se.method)==1){
      return(myStdDev)
    } else {
      res=list(SD=myStdDev)
      # for each of the method specified in se.method, compute the standard error
      for(mymethod in se.method){
        res[[mymethod]]=EstimatorSE(R, estimator.fun = "SD", se.method = mymethod)
      }
      return(res)
    }
  }
}
