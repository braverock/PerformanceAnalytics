#' @title Normalized Calmar ratio
#'  
#' @description Normalized Calmar and Sterling Ratios are yet another method of creating a
#' risk-adjusted measure for ranking investments similar to the Sharpe Ratio.
#' 
#' @details 
#' Both the Normalized Calmar and the Sterling ratio are the ratio of annualized return
#' over the absolute value of the maximum drawdown of an investment.
#' \deqn{Sterling Ratio  =   [Return over (0,T)]/[max Drawdown(0,T)]}
#' It is also \emph{traditional} to use a three year return series for these
#' calculations, although the functions included here make no effort to
#' determine the length of your series.  If you want to use a subset of your
#' series, you'll need to truncate or subset the input data to the desired
#' length.
#' It is also traditional to use a three year return series for these
#' calculations, although the functions included here make no effort to
#' determine the length of your series.  If you want to use a subset of your
#' series, you'll need to truncate or subset the input data to the desired
#' length.
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param excess for Sterling Ratio, excess amount to add to the max drawdown,
#' traditionally and default .1 (10\%)
#' @author Brian G. Peterson , Peter Carl , Shubhankit Mohan
#' @references Bacon, Carl. \emph{Magdon-Ismail, M. and Amir Atiya, Maximum drawdown. Risk Magazine, 01 Oct 2004.
#' @keywords ts multivariate distribution models
#' @examples
#' 
#'     data(managers)
#'     CalmarRatio.Norm(managers[,1,drop=FALSE])
#'     CalmarRatio.Norm(managers[,1:6]) 
#' @export 
#' @rdname CalmarRatio.Norm

CalmarRatio.Norm <- function (R, tau = 1,scale = NA)
{ # @author Brian G. Peterson
  
  # DESCRIPTION:
  # Inputs:
  # Ra: in this case, the function anticipates having a return stream as input,
  #    rather than prices.
  # tau : scaled Time in Years
  # scale: number of periods per year
  # Outputs:
  # This function returns a Calmar Ratio
  
  # FUNCTION:
  
  R = checkData(R)
  if(is.na(scale)) {
    freq = periodicity(R)
    switch(freq$scale,
           minute = {stop("Data periodicity too high")},
           hourly = {stop("Data periodicity too high")},
           daily = {scale = 252},
           weekly = {scale = 52},
           monthly = {scale = 12},
           quarterly = {scale = 4},
           yearly = {scale = 1}
    )
  }
  Time = nyears(R)
  annualized_return = Return.annualized(R, scale=scale)
  drawdown = abs(maxDrawdown(R))
  result = (annualized_return/drawdown)*(QP.Norm(R,Time)/QP.Norm(R,tau))*(tau/Time)
  rownames(result) = "Normalized Calmar Ratio"
  return(result)
}
