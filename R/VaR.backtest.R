#' @title VaR Backtest
#'
#' @description
#' Provides a simple binomial backtest for a Value at Risk (VaR) model.
#'
#' This function performs a Kupiec Proportion of Failures (POF) test to evaluate
#' whether the number of times returns breached the VaR threshold matches the expected
#' frequency \code{1-p}.
#'
#' The original implementation was provided in the archived \kbd{VaR} package by Talgat Daniyarov.
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' actual asset returns
#' @param VaR estimated Value at Risk scalar or vector corresponding to the same periods
#' @param p confidence level for calculation, default p=.95
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{expected_exceedances} the expected number of VaR breaches.
#'   \item \code{actual_exceedances} the number of actual VaR breaches.
#'   \item \code{p.value} the p-value from the binomial proportion test.
#' }
#'
#' @author Brian G. Peterson, Talgat Daniyarov
#' @references Daniyarov, T. \emph{VaR: Value at Risk estimation}. CRAN Archive. 2004.
#' \url{https://cran.r-project.org/src/contrib/Archive/VaR/}
#' @seealso \code{\link{VaR}} \cr
#' @examples
#'
#' data(edhec)
#' # calculate VaR at 95 percent confidence
#' v <- as.numeric(VaR(edhec[, 1], p = 0.95, method = "historical"))
#'
#' # evaluate the backtest
#' VaR.backtest(edhec[, 1], v, p = 0.95)
#'
#' @export
VaR.backtest <- function(R, VaR, p = 0.95) {
  R <- checkData(R)

  if (length(VaR) == 1) {
    VaR <- rep(as.numeric(VaR), length(R))
  }

  n <- length(R)

  actual_fails <- sum(R < VaR, na.rm = TRUE)

  # Kupiec proportion of failures test
  prop_test <- prop.test(actual_fails, n, p = 1 - p)

  result <- list(
    expected_exceedances = n * (1 - p),
    actual_exceedances = actual_fails,
    p.value = prop_test$p.value
  )

  return(result)
}
