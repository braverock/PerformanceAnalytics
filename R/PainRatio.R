#' Pain ratio of the return distribution
#'
#' To calculate Pain ratio we divide the difference of the portfolio return
#' and the risk free rate by the Pain index
#'
#' \deqn{Pain ratio = \frac{r_P - r_F}{\sum^{n}_{i=1} \frac{\mid D'_i \mid}{n}}}
#' {Pain ratio = (rp - rf) / Pain index}
#'
#' where \eqn{r_P} is the annualized portfolio return, \eqn{r_F} is the risk free
#' rate, \eqn{n} is the number of observations of the entire series, \eqn{D'_i} is
#' the drawdown since previous peak in period i
#'
#' @aliases PainRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rf risk free rate, in same period as your returns
#' @param period number of periods in a year monthly scale = 12, quarterly = 4)
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.91
#' 
#' @keywords ts multivariate distribution models
#' @examples
#' data(portfolio_bacon[,1])
#' print(PainRatio(portfolio_bacon)) #expected 2.59
#'
#' data(managers)
#' print(PainRatio(100*managers['1996']))
#' print(PainRatio(100*managers['1996',1])) 
#'
#' @export 


PainRatio <- function (R, Rf = 0, period = 12, ...) 
{
    R0 <- R
    R = checkData(R, method="matrix")

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       PI = PainIndex(R)
       R = na.omit(R)
       n = length(R)
       Rp = (prod(1+R/100)^(period/length(R))-1)*100
       result = (Rp - Rf) / PI
       reclass(result, R0)
       return(result)
    }
    else {
        R = checkData(R)
        result = apply(R, MARGIN = 2, PainRatio, MAR = MAR, Rf = Rf, period = period, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("Pain Ratio (Rf = ",Rf,")", sep="")
        return(result)
    }
}