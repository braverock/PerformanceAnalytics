#' M squared of the return distribution
#'
#' M squared is a risk adjusted return useful to judge the size of relative
#' performance between differents portfolios. With it you can compare portfolios
#' with different levels of risk.
#'
#' \deqn{M^2 = r_P + SR * (\sigma_M - \sigma_P) = (r_P - r_F) * \frac{\sigma_M}{\sigma_P} + r_F}{M squared = Rp + SR * (Market risk - Portfolio risk) = (Rp - Rf) * Market risk / Portfolio risk + Rf}
#'
#' where \eqn{r_P} is the portfolio return annualized, \eqn{\sigma_M} is the market
#' risk and \eqn{\sigma_P} is the portfolio risk
#'
#' @aliases MSquared
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset return
#' @param Rb return vector of the benchmark asset 
#' @param Rf risk free rate, in same period as your returns
#' @param Period the number of return in a year in the asset return 
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.67-68
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(MSquared(portfolio_bacon[,1], portfolio_bacon[,2])) #expected 0.1068
#'
#' data(managers)
#' print(MSquared(managers['1996',1], managers['1996',8]))
#' print(MSquared(managers['1996',1:5], managers['1996',8]))
#'
#' @export 
MSquared <-
function (Ra, Rb, Rf = 0, Period = 12, ...)
{
    Ra = checkData(Ra)
    Rb = checkData(Rb)

    if (ncol(Ra)==1 || is.null(Ra) || is.vector(Ra)) {
    calcul = FALSE    
     for (i in (1:length(Ra))) {
     	 if (!is.na(Ra[i])) {
     	    calcul = TRUE
	 }
      }

     if (calcul) {
        Rp = Return.annualized(Ra)
     	sigp = sqrt(var(Ra)*(length(Ra)-1)/length(Ra))*sqrt(Period)
     	sigm = sqrt(var(Rb)*(length(Rb)-1)/length(Rb))*sqrt(Period)
        result = (Rp - Rf) * sigp / sigm + Rf
     }    
     else {
        result = NA
     }
      return(result)
    }
    else {
        result = apply(Ra, MARGIN = 2, MSquared, Rb = Rb, Rf = Rf, Period = Period, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("MSquared (Risk free = ",Rf,")", sep="")
        return(result)
    }
}
