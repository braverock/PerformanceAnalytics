#' Bernardo and Ledoit ratio of the return distribution
#'
#' To calculate Bernado and Ledoit ratio we take the sum of the subset of
#' returns that are above 0 and we divide it by the opposite of the sum of
#' the subset of returns that are below 0
#'
#' \deqn{BernadoLedoitratio(R) = \frac{\frac{1}{n}\sum^{n}_{t=1}{max(R_{t},0)}}{\frac{1}{n}\sum^{n}_{t=1}
#' {max(-R_{t},0)}}}{BernadoLedoitratio(R) = 1/n*sum
#' (t=1..n)(max(R(t),0)) / 1/n*sum(t=1..n)(max(-R(t),0))}
#'
#' where \eqn{n} is the number of observations of the entire series
#' 
#' @aliases BernardoLedoitRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.95
#' 
#' @keywords ts multivariate distribution models
#' @examples
#' data(portfolio_bacon)
#' print(BernadoLedoitratio(portfolio_bacon)) #expected 1.78
#'
#' data(managers)
#' print(BernadoLedoitratio(managers['1996']))
#' print(BernadoLedoitratio(managers['1996',1])) #expected 4.598
#'
#' @export 

BernardoLedoitratio <- function (R, ...)
{
    R0 <- R
    R = checkData(R, method="matrix")

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       R = na.omit(R)
       r1 = subset(R, R > 0)
       r2 = subset(R, R < 0)
       result = sum(r1)/-sum(r2)    
       reclass(result, R0)
       return(result)
    }  
    else {
        R = checkData(R)
        result = apply(R, MARGIN = 2, BernardoLedoitratio, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("Bernardo and Ledoit ratio", sep="")
        return(result)
    }
}