#' Skewness-Kurtosis ratio of the return distribution
#'
#' Skewness-Kurtosis ratio is the division of Skewness by Kurtosis.
#' 
#' It is used in conjunction with the Sharpe ratio to rank portfolios.
#' The higher the rate the better.
#'
#' \deqn{ SkewnessKurtosisRatio(R , MAR) = \frac{S}{K}}
#' {SkewnessKurtosisRatio(R, MAR) = S/K}
#'
#' where \eqn{S} is the skewness and \eqn{K} is the Kurtosis
#'
#' @aliases Skewness-Kurtosis ratio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.100
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' print(SkewnessKurtosisRatio(portfolio_bacon)) #expected -0.0347
#'
#' data(managers)
#' print(SkewnessKurtosisRatio(managers['1996']))
#' print(SkewnessKurtosisRatio(managers['1996',1])
#'
#' @export 
SkewnessKurtosisRatio <-
function (R, ...)
{
    R0 <- R
    R = checkData(R, method="matrix")

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       R = na.omit(R)
	result = skewness(R) / kurtosis(R, method = "moment")
	reclass(result, R0)
        return(result)
    }
    else {
        R = checkData(R)
        result = apply(R, MARGIN = 2, SkewnessKurtosisRatio, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("SkewnessKurtosisRatio", sep="")
        return(result)
    }
}