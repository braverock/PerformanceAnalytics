#' Appraisal ratio of the return distribution
#'
#' Appraisal ratio is the Jensen's alpha adjusted for specific risk. The numerator
#' is divided by specific risk instead of total risk.
#'
#' Modified Jensen's alpha is Jensen's alpha divided by beta.
#'
#' Alternative Jensen's alpha is Jensen's alpha divided by systematic risk.
#'
#' \deqn{Appraisal ratio = \frac{\alpha}{\sigma_{\epsilon}}}{Appraisal ratio = Jensen's alpha / specific risk}
#'
#' \deqn{Modified Jensen's alpha = \frac{\alpha}{\beta}}{Modified Jensen's alpha = Jensen's alpha / beta}
#'
#' \deqn{Alternative Jensen's alpha = \frac{\alpha}{\sigma_S}}{Alternative Jensen's alpha = Jensen's alpha / systematic risk}
#'
#' where \eqn{alpha} is the Jensen's alpha, \eqn{\sigma_{epsilon}} is the specific risk,
#' \eqn{\sigma_S} is the systematic risk.
#'
#' @aliases AppraisalRatio
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param method is one of "appraisal" to calculate appraisal ratio, "modified" to
#' calculate modified Jensen's alpha or "alternative" to calculate alternative
#' Jensen's alpha.
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.77
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(AppraisalRatio(portfolio_bacon[,1], portfolio_bacon[,2], method="appraisal")) #expected -0.430
#' print(AppraisalRatio(portfolio_bacon[,1], portfolio_bacon[,2], method="modified")) 
#' print(AppraisalRatio(portfolio_bacon[,1], portfolio_bacon[,2], method="alternative"))
#'
#' data(managers)
#' print(AppraisalRatio(managers['1996',1], managers['1996',8]))
#' print(AppraisalRatio(managers['1996',1:5], managers['1996',8]))
#'
#' @export 

AppraisalRatio <-
function (Ra, Rb, Rf = 0, method = c("appraisal", "modified", "alternative"), ...)
{
    method = method[1]

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
        switch(method,
            appraisal   = {
	    		epsilon = Ra - Rb * CAPM.beta(Ra,Rb,Rf) - CAPM.alpha(Ra,Rb,Rf)
        		specifikRisk = sqrt(sum((epsilon - mean(epsilon))^2)/length(epsilon))*sqrt(Period)
        		result = CAPM.jensenAlpha(Ra,Rb,Rf,Period)/specifikRisk
	    },
            modified = {result = CAPM.jensenAlpha(Ra,Rb,Rf,Period)/CAPM.beta(Ra,Rb,Rf)},
	    alternative = {result = CAPM.jensenAlpha(Ra,Rb,Rf,Period)/SystematicRisk(Ra,Rb,Rf, Period)}
        ) # end switch
     }    
     else {
        result = NA
     }
      return(result)
    }
    else {
        Ra = checkData(Ra)
        result = apply(Ra, MARGIN = 2, AppraisalRatio, Rb = Rb, Rf = Rf, method = method,...)
        result<-t(result)
        colnames(result) = colnames(Ra)
        switch(method,
            appraisal   = {rownames(result) = paste("Appraisal ratio (Risk free = ",Rf,")", sep="")},
            modified = {rownames(result) = paste("Modified Jensen's alpha (Risk free = ",Rf,")", sep="")},
	    alternative = {rownames(result) = paste("Alternative Jensen's alpha (Risk free = ",Rf,")", sep="")}
        ) # end switch
        return(result)
    }
}
