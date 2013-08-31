#' calculate a Normalized Calmar or Sterling reward/risk ratio
#'  
#' Normalized Calmar and Sterling Ratios are yet another method of creating a
#' risk-adjusted measure for ranking investments similar to the
#' \code{\link{SharpeRatio}}.
#' 
#' Both the Normalized Calmar and the Sterling ratio are the ratio of annualized return
#' over the absolute value of the maximum drawdown of an investment. The
#' Sterling ratio adds an excess risk measure to the maximum drawdown,
#' traditionally and defaulting to 10\%.
#' 
#' It is also traditional to use a three year return series for these
#' calculations, although the functions included here make no effort to
#' determine the length of your series.  If you want to use a subset of your
#' series, you'll need to truncate or subset the input data to the desired
#' length.
#' 
#' Many other measures have been proposed to do similar reward to risk ranking.
#' It is the opinion of this author that newer measures such as Sortino's
#' \code{\link{UpsidePotentialRatio}} or Favre's modified
#' \code{\link{SharpeRatio}} are both \dQuote{better} measures, and
#' should be preferred to the Calmar or Sterling Ratio.
#' 
#' @aliases Normalized.CalmarRatio Normalized.SterlingRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param tau Time Scale Translations Factor
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @author Shubhankit
#' @seealso 
#' \code{\link{Return.annualized}}, \cr 
#' \code{\link{maxDrawdown}}, \cr
#' \code{\link{SharpeRatio.modified}}, \cr 
#' \code{\link{UpsidePotentialRatio}}
#' @references Bacon, Carl. \emph{Magdon-Ismail, M. and Amir Atiya, Maximum drawdown. Risk Magazine, 01 Oct 2004.
#' @keywords ts multivariate distribution models
#' @examples
#' 
#'     data(managers)
#'     Normalized.CalmarRatio(managers[,1,drop=FALSE])
#'     Normalized.CalmarRatio(managers[,1:6]) 
#'     Normalized.SterlingRatio(managers[,1,drop=FALSE])
#'     Normalized.SterlingRatio(managers[,1:6])
#' 
#' @rdname QP.Norm
#' QP function fo calculation of Sharpe Ratio
#' @export 
QP.Norm <- function (R, tau,scale = NA)
{
  Sharpe= as.numeric(SharpeRatio.annualized(R))
  return(.63519+(.5*log(tau))+log(Sharpe))
}
