#' Appraisal ratio of the return distribution
#'
#' Appraisal ratio is the Jensen's alpha adjusted for systemeatic risk. The numerator
#' is divided by specific risk instead of total risk.
#'
#' \deqn{Appraisal ratio = \frac{alpha}{\sigma_{\epsilon}}}{Appraisal ratio = Jensen's alpha / specific risk}
#'
#' where \eqn{alpha} is the Jensen's alpha and \eqn{\sigma_{epsilon}} is the specific risk.
#'
#' @aliases AppraisalRatio
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param period number of periods in a year monthly scale = 12, quarterly = 4)
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.72
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(AppraisalRatio(portfolio_bacon[,1], portfolio_bacon[,2])) #expected -0.0952
#'
#' data(managers)
#' print(AppraisalRatio(managers['1996',1], managers['1996',8]))
#' print(AppraisalRatio(managers['1996',1:5], managers['1996',8]))
#'
#' @export 

AppraisalRatio <-
function (Ra, Rb, Rf = 0, period = 12, ...)
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

     if (calcul) {
        Period = Frequency(Ra)
        result = CAPM.jensenAlpha(Ra,Rb,Rf,Period)/SystematicRisk(Ra,Rb,Rf,Period)
     }    
     else {
        result = NA
     }
      return(result)
    }
    else {
        Ra = checkData(Ra)
        result = apply(Ra, MARGIN = 2, AppraisalRatio, Rb = Rb, Rf = Rf, ...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        rownames(result) = paste("Appraisal ratio (Risk free = ",Rf,")", sep="")
        return(result)
    }
}
