#' Time-varying conditional single factor model beta
#' 
#' CAPM is estimated assuming that betas and alphas change over time. It is 
#' assumed that the market prices of securities fully reflect readily available
#' and public information. A matrix of market information variables, \eqn{Z}
#' measures this information. Possible variables in \eqn{Z} could be the
#' divident yield, Tresaury yield, etc. The betas of stocks and managed
#' portfolios are allowed to change with market conditions:
#' 
#' \deqn{\beta_{p}(z_{t})=b_{0p}+B_{p}'z_{t}}{beta(zt) = b0 + Bp'zt}
#' 
#' where \eqn{z_{t}=Z_{t}-E[Z]}{zt = Zt - E[Z]} 
#' 
#' - a normalized vector of the deviations of \eqn{Z_{t}}{Zt}, \eqn{B_{p}}{Bp} 
#' 
#' - a vector with the same dimension as \eqn{Z_{t}}{Zt}. 
#' 
#' The coefficient \eqn{b_{0p}}{b0} can be 
#' interpreted as the "average beta" or the beta when all infromation variables
#' are at their means. The elements of \eqn{B_{p}}{Bp} measure the sensitivity 
#' of the conditional beta to the deviations of the \eqn{Z_{t}}{Zt} from their
#' means. 
#' In the similar way the time-varying conditional alpha is modeled:
#' \deqn{\alpha_{pt}=\alpha_{p}(z_{t})=\alpha_{0p}+A_{p}'z_{t}}{alpha(zt) = 
#' a0 + Ap'zt}
#' The modified regression is therefore:
#' \deqn{r_{pt+1}=\alpha_{0p}+A_{p}'z_{t}+b_{0p}r_{bt+1}+B_{p}'[z_{t}r_{bt+1}]+
#' \mu_{pt+1}}
#' 
#' @aliases SFM.dynamic
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' the asset returns
#' @param Rb an xts, vector, matrix, data frame, timeSeries or zoo object of 
#' the benchmark asset return
#' @param Rf risk free rate, in same period as your returns
#' @param Z an xts, vector, matrix, data frame, timeSeries or zoo object of 
#' k variables that reflect public information
#' @param lags number of lags before the current period on which the alpha and
#' beta are conditioned
#' @param \dots any other passthrough parameters
#' @author Andrii Babii
#' @seealso \code{\link{CAPM.beta}}
#' @references J. Christopherson, D. Carino, W. Ferson. \emph{Portfolio 
#' Performance Measurement and Benchmarking}. 2009. McGraw-Hill. Chapter 12. 
#' \cr Wayne E. Ferson and Rudi Schadt, "Measuring Fund Strategy and 
#' Performance in Changing Economic Conditions," \emph{Journal of Finance}, 
#' vol. 51, 1996, pp.425-462 \cr
#' @examples
#' 
#' data(managers)
#' CAPM.dynamic(managers[,1,drop=FALSE], managers[,8,drop=FALSE], 
#'              Rf=.035/12, Z=managers[, 9:10])
#' 
#' CAPM.dynamic(managers[80:120,1:6], managers[80:120,7,drop=FALSE], 
#'              Rf=managers[80:120,10,drop=FALSE], Z=managers[80:120, 9:10])
#'              
#' CAPM.dynamic(managers[80:120,1:6], managers[80:120,8:7],
#'               managers[80:120,10,drop=FALSE], Z=managers[80:120, 9:10])
#' 
#' @rdname CAPM.dynamic 
#' @export CAPM.dynamic SFM.dynamic 
CAPM.dynamic <- SFM.dynamic <- function (Ra, Rb, Rf = 0, Z, lags = 1, ...)
{ # @author Andrii Babii
    
    # FUNCTION
  
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    Z = checkData(Z)
    Z = na.omit(Z)
    if (!is.null(dim(Rf))) 
      Rf = checkData(Rf)
    Ra.ncols = NCOL(Ra)
    Rb.ncols = NCOL(Rb)
    pairs = expand.grid(1:Ra.ncols)
    
    xRa = Return.excess(Ra, Rf)[1:(nrow(Ra) - 1)]
    xRb = Return.excess(Rb, Rf)[1:(nrow(Rb) - 1)]
    z = Z - matrix(rep(mean(Z), nrow(Z)), nrow(Z), ncol(Z), byrow = TRUE)
    # Construct the matrix with information regressors (lagged values)
    inform = lag.xts(z)
    if (lags > 1){
      for (i in 2:lags) {
        inform = cbind(inform, lag.xts(z, i))
      }
    }
    z = inform[(lags + 1):nrow(z), ]
        
    dynamic <- function (xRa, xRb, z){
      y = xRa[1:nrow(z)]
      X = cbind(z, coredata(xRb[1:nrow(z)]), z * matrix(rep(xRb[1:nrow(z)], ncol(z)), nrow(z), ncol(z)))
      X.df = as.data.frame(X)
      model = lm(xRa[1:nrow(z)] ~ 1 + ., data = X.df)
      return(coef(model))
    }
    result = apply(pairs, 1, FUN = function(n, xRa, xRb, z) 
      dynamic(xRa[, n[1]], xRb[, 1], z), xRa = xRa, xRb = xRb, z = z)
    result = t(result)
    
    if (ncol(Rb) > 1){
      for (i in 2:ncol(xRb)){
        res = apply(pairs, 1, FUN = function(n, xRa, xRb, z) 
          dynamic(xRa[, n[1]], xRb[, i], z), xRa = xRa, xRb = xRb, z = z)
        res = t(res)
        result = rbind(result, res)
      }
    }
    
    a = paste(rep(colnames(Z), lags), "alpha at t -", expand.grid(1:ncol(Z), 1:lags)[, 2])
    b = paste(rep(colnames(Z), lags), "beta at t -", expand.grid(1:ncol(Z), 1:lags)[, 2])
    colnames(result) = c("Average alpha", a, "Average beta", b)
    rownames(result) = paste(rep(colnames(Ra), ncol(Rb)), "to",  rep(colnames(Rb), each = ncol(Ra)))
    return(result)
}