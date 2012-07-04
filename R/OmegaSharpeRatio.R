#' Omega-Sharpe ratio of the return distribution
#'
#' The omega ratio is a conversion of the omega ratio to a ranking statistic 
#' in familiar form to the Sharpe ratio.
#'
#' To calculate the Omega-Sharpe ration we subtract the target (or Minimum
#' Acceptable Returns (MAR)) return from the portfolio return and we divide
#' it by the opposite of the Downside Deviation.
#'
#' \deqn{OmegaSharpeRatio(R,MAR) = \frac{r_p - r_t}{\sum^n_{t=1}\frac{max(r_t - r_i, 0)}{n}}}
#' {OmegaSharpeRatio(R,MAR) = (Rp - Rt) / -DownsidePotential(R,MAR)}
#'
#' where \eqn{n} is the number of observations of the entire series
#' 
#' @aliases OmegaSharpeRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param MAR Minimum Acceptable Return, in the same periodicity as your
#' returns
#' @param method one of "full" or "subset", indicating whether to use the
#' length of the full series or the length of the subset of the series below
#' the MAR as the denominator, defaults to "subset"
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008, p.95
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' MAR = 0.5
#' print(OmegaSharpeRatio(portfolio_bacon, MAR)) #expected 0.29
#'
#' MAR = 0
#' data(managers)
#' print(OmegaSharpeRatio(managers['1996'], MAR))
#' print(OmegaSharpeRatio(managers['1996',1], MAR)) #expected 3.60
#'
#' @export 

OmegaSharpeRatio <-
function (R, MAR = 0, ...)
{
    R0 <- R
    R = checkData(R, method="matrix")

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       R = na.omit(R)
       r = subset(R, R > MAR)

        if(!is.null(dim(MAR))){
            if(is.timeBased(index(MAR))){
                MAR <-MAR[index(r)] #subset to the same dates as the R data
            } 
	    else{
                MAR = mean(checkData(MAR, method = "vector"))
                # we have to assume that Ra and a vector of Rf passed in for MAR both cover the same time period
            }
        }
	result = (UpsideRisk(R,MAR,stat="potential") - DownsidePotential(R,MAR))/(DownsidePotential(R,MAR))
	reclass(result, R0)
        return(result)
    }
    else {
        R = checkData(R)
        result = apply(R, MARGIN = 2, OmegaSharpeRatio, MAR = MAR, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("OmegaSharpeRatio (MAR = ",MAR,"%)", sep="")
        return(result)
    }
}