#' Jensen's alpha of the return distribution
#'
#' The Jensen's alpha is the intercept of the regression equation in the Capital
#' Asset Pricing Model and is in effect the exess return adjusted for systematic risk.
#'
#' \deqn{\alpha = r_p - r_f - \beta_p * (b - r_f)} 
#' {alpha = r_p - r_f - beta_p * (b - r_f)}
#'
#' where \eqn{r_f} is the systematic risk, \eqn{\beta_r} is the regression beta,
#' \eqn{r_p} is the portfolio return and b is the benchmark return
#'
#' @aliases Regression epsilon
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.72
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(CAPM.jensenAlpha(portfolio_bacon[,1], portfolio_bacon[,2])) #expected -3.14938
#'
#' data(managers)
#' print(CAPM.jensenAlpha(managers['1996',1], managers['1996',8]))
#' print(CAPM.jensenAlpha(managers['1996',1:5], managers['1996',8]))
#'
#' @export 

CAPM.jensenAlpha <-
function (Ra, Rb, Rf = 0, ...)
{
    calcul = FALSE
    Ra = checkData(Ra, method="matrix")
    Rb = checkData(Rb, method="matrix")

    if (ncol(Ra)==1 || is.null(Ra) || is.vector(Ra)) {
    
     Rp = (prod(0.01*Ra+1)-1)*100 #portfolio total return
     Rpb = (prod(0.01*Rb+1)-1)*100 #benchmark total return
     for (i in (1:length(Ra))) {
     	 if (!is.na(Ra[i])) {
     	    calcul = TRUE
	 }
      }

     if (calcul) {
        result = Rp - Rf - CAPM.beta(Ra,Rb,Rf) * (Rpb - Rf) 
     }    
     else {
        result = NA
     }
      return(result)
    }
    else {
        Ra = checkData(Ra)
        result = apply(Ra, MARGIN = 2, CAPM.jensenAlpha, Rb = Rb, Rf = Rf, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("Jensen's Alpha (Risk free = ",Rf,")", sep="")
        return(result)
    }
}
