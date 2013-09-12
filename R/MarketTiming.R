#' Market timing models
#' 
#' Allows to estimate Treynor-Mazuy or Merton-Henriksson market timing model.
#' The Treynor-Mazuy model is essentially a quadratic extension of the basic
#' CAPM. It is estimated using a multiple regression. The second term in the
#' regression is the value of excess return squared. If the gamma coefficient
#' in the regression is positive, then the estimated equation describes a 
#' convex upward-sloping regression "line". The quadratic regression is:
#' \deqn{R_{p}-R_{f}=\alpha+\beta (R_{b} - R_{f})+\gamma (R_{b}-R_{f})^2+
#' \varepsilon_{p}}{Rp - Rf = alpha + beta(Rb -Rf) + gamma(Rb - Rf)^2 + 
#' epsilonp}
#' \eqn{\gamma}{gamma} is a measure of the curvature of the regression line.
#' If \eqn{\gamma}{gamma} is positive, this would indicate that the manager's
#' investment strategy demonstrates market timing ability.
#' 
#' The basic idea of the Merton-Henriksson test is to perform a multiple 
#' regression in which the dependent variable (portfolio excess return and a 
#' second variable that mimics the payoff to an option). This second variable 
#' is zero when the market excess return is at or below zero and is 1 when it 
#' is above zero:
#' \deqn{R_{p}-R_{f}=\alpha+\beta (R_{b}-R_{f})+\gamma D+\varepsilon_{p}}{Rp - 
#' Rf = alpha + beta * (Rb - Rf) + gamma * D + epsilonp}
#' where all variables are familiar from the CAPM model, except for the 
#' up-market return \eqn{D=max(0,R_{b}-R_{f})}{D = max(0, Rb - Rf)} and market 
#' timing abilities \eqn{\gamma}{gamma}
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' the asset returns
#' @param Rb an xts, vector, matrix, data frame, timeSeries or zoo object of 
#' the benchmark asset return
#' @param Rf risk free rate, in same period as your returns
#' @param method used to select between Treynor-Mazuy and Henriksson-Merton
#' models. May be any of: \itemize{ \item TM - Treynor-Mazuy model, 
#' \item HM - Henriksson-Merton model} By default Treynor-Mazuy is selected
#' @param \dots any other passthrough parameters
#' @author Andrii Babii, Peter Carl
#' @seealso \code{\link{CAPM.beta}}
#' @references J. Christopherson, D. Carino, W. Ferson. \emph{Portfolio 
#' Performance Measurement and Benchmarking}. 2009. McGraw-Hill, p. 127-133.
#' \cr J. L. Treynor and K. Mazuy, "Can Mutual Funds Outguess the Market?" 
#' \emph{Harvard Business Review}, vol44, 1966, pp. 131-136 
#' \cr Roy D. Henriksson and Robert C. Merton, "On Market Timing and Investment
#' Performance. II. Statistical Procedures for Evaluating Forecast Skills," 
#' \emph{Journal of Business}, vol.54, October 1981, pp.513-533 \cr
#' @examples
#' 
#' data(managers)
#' MarketTiming(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf=.035/12, method = "HM")
#' MarketTiming(managers[80:120,1:6], managers[80:120,7,drop=FALSE], managers[80:120,10,drop=FALSE])
#' MarketTiming(managers[80:120,1:6], managers[80:120,8:7], managers[80:120,10,drop=FALSE], method = "TM")
#'
#' @export
MarketTiming <- function (Ra, Rb, Rf = 0, method = c("TM", "HM"))
{ # @author Andrii Babii, Peter Carl
  
    # FUNCTION
  
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if (!is.null(dim(Rf))) 
      Rf = checkData(Rf)
    Ra.ncols = NCOL(Ra)
    Rb.ncols = NCOL(Rb)
    pairs = expand.grid(1:Ra.ncols, 1)
    method = method[1]
    xRa = Return.excess(Ra, Rf)
    xRb = Return.excess(Rb, Rf)

    mt <- function (xRa, xRb)
    {
      switch(method,
             "HM" = { S = xRb > 0 },
             "TM" = { S = xRb }
      )
      R = merge(xRa, xRb, xRb*S)
      R.df = as.data.frame(R)
      model = lm(R.df[, 1] ~ 1 + ., data = R.df[, -1])
      return(coef(model))
    }
  
    result = apply(pairs, 1, FUN = function(n, xRa, xRb) 
      mt(xRa[, n[1]], xRb[, 1]), xRa = xRa, xRb = xRb)
    result = t(result)
    
    if (ncol(Rb) > 1){
      for (i in 2:ncol(xRb)){
        res = apply(pairs, 1, FUN = function(n, xRa, xRb) 
          mt(xRa[, n[1]], xRb[, i]), xRa = xRa, xRb = xRb)
        res = t(res)
        result = rbind(result, res)
      }
    }
  
    rownames(result) = paste(rep(colnames(Ra), ncol(Rb)), "to",  rep(colnames(Rb), each = ncol(Ra)))
    colnames(result) = c("Alpha", "Beta", "Gamma")
    return(result)
}