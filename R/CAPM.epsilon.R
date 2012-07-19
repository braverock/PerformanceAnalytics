#' Regression epsilon of the return distribution
#'
#' The regression epsilon is an error term measuring the vertical distance between
#' the return predicted by the equation and the real result.
#'
#' \deqn{\epsilon_r = r_p - \alpha_r - \beta_r * b} 
#' {epsilon_r = r_p - alpha_r - beta_r * b}
#'
#' where \eqn{\alpha_r} is the regression alpha, \eqn{\beta_r} is the regression beta,
#' \eqn{r_p} is the portfolio return and b is the benchmark return
#'
#' @aliases Regression epsilon
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param period number of periods in a year monthly scale = 12, quarterly = 4)
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.71
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(CAPM.epsilon(portfolio_bacon[,1], portfolio_bacon[,2])) #expected -1.31
#'
#' data(managers)
#' print(CAPM.epsilon(managers['1996',1], managers['1996',8]))
#' print(CAPM.epsilon(managers['1996',1:5], managers['1996',8]))
#'
#' @export 

CAPM.epsilon <-
function (Ra, Rb, Rf = 0, period=12, ...)
{
    Ra = checkData(Ra, method="matrix")
    Rb = checkData(Rb, method="matrix")

    if (ncol(Ra)==1 || is.null(Ra) || is.vector(Ra)) {
    calcul = FALSE
       for (i in (1:length(Ra))) {
          if (!is.na(Ra[i])) {
             calcul = TRUE
          }
        }
     Ra = na.omit(Ra)
     Rb = na.omit(Rb)
     print(Ra)
     print(Rb)
     Rp = (prod(1+Ra/100)^(period/length(Ra))-1)*100
     Rpb =  (prod(1+Rb/100)^(period/length(Rb))-1)*100 #benchmark total return
     print(Rp)
     print(Rpb)

        if (calcul) {
	   print(Rf)
	   print(CAPM.alpha(Ra,Rb,Rf))
           result = Rf + Rp - CAPM.alpha(Ra,Rb,Rf) - (Rpb - Rf) * CAPM.beta(Ra,Rb,Rf) 
	   print(result)
        }   
        else {
           result = NA
        }
        return(result)
    }
    else {
    	Ra = checkData(Ra)
        result = apply(Ra, MARGIN = 2, CAPM.epsilon, Rb = Rb, Rf = Rf, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("Regression epsilon (Risk free = ",Rf,")", sep="")
        return(result)
    }
}
