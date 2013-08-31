#' QP function for calculation of Sharpe Ratio
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param tau Time Scale Translations Factor
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' @seealso 
#' \code{\link{CalmarRatio.Norm}}, \cr 
#' @export 
QP.Norm <- function (R, tau,scale = NA)
{
  Sharpe= as.numeric(SharpeRatio.annualized(R))
  return(.63519+(.5*log(tau))+log(Sharpe))
}
