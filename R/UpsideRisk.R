#' upside risk, variance and potential of the return distribution
#'
#' Upside Risk is the similar of semideviation taking the return above the
#' Minimum Acceptable Return instead of using the mean return or zero.

#' To calculate it, we take the subset of returns that are more than the target
#' (or Minimum Acceptable Returns (MAR)) returns and take the differences of
#' those to the target.  We sum the squares and divide by the total number of
#' returns and return the square root.
#'
#' \deqn{ UpsideRisk(R , MAR) = \sqrt{\sum^{n}_{t=1}\frac{
#' max[(R_{t} - MAR), 0]^2}{n}}}{UpsideRisk(R, MAR) = sqrt(1/n * sum(t=1..n)
#' ((max(R(t)-MAR, 0))^2))}
#'
#' \deqn{ UpsideVariance(R, MAR) = \sum^{n}_{t=1}\frac{max[(R_{t} - MAR), 0]^2} {n}}{UpsideVariance(R, MAR) = 1/n * sum(t=1..n)((max(R(t)-MAR, 0))^2)}
#'
#' \deqn{UpsidePotential(R, MAR) = \sum^{n}_{t=1}\frac{max[(R_{t} - MAR), 0]} {n}}{DownsidePotential(R, MAR) =  1/n * sum(t=1..n)(max(R(t)-MAR, 0))}
#'
#' where \eqn{n} is either the number of observations of the entire series or
#' the number of observations in the subset of the series falling below the
#' MAR.
#'
#' @aliases UpsideRisk
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param MAR Minimum Acceptable Return, in the same periodicity as your
#' returns
#' @param method one of "full" or "subset", indicating whether to use the
#' length of the full series or the length of the subset of the series below
#' the MAR as the denominator, defaults to "full"
#' @param stat one of "risk", "variance" or "potential" indicating whether
#' to return the Upside risk, variance or potential
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008
#' 
#' @keywords ts multivariate distribution models
#' @examples
#'
#' data(portfolio_bacon)
#' MAR = 0.005
#' print(UpsideRisk(portfolio_bacon[,1], MAR, stat="risk")) #expected 0.02937
#' print(UpsideRisk(portfolio_bacon[,1], MAR, stat="variance")) #expected 0.08628
#' print(UpsideRisk(portfolio_bacon[,1], MAR, stat="potential")) #expected 0.01771
#'
#' MAR = 0
#' data(managers)
#' print(UpsideRisk(managers['1996'], MAR, stat="risk"))
#' print(UpsideRisk(managers['1996',1], MAR, stat="risk")) #expected 1.820
#'
#' @export 

UpsideRisk <-
function (R, MAR = 0, method=c("full","subset"), stat=c("risk","variance","potential"), ...)
{
    method = method[1]
    stat = stat[1]

    R = checkData(R, method="matrix")

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       R = na.omit(R)
       r = R[which(R > MAR)]

        if(!is.null(dim(MAR))){
            if(is.timeBased(index(MAR))){
                MAR <-MAR[index(r)] #subset to the same dates as the R data
            } 
	    else{
                MAR = mean(checkData(MAR, method = "vector"))
                # we have to assume that Ra and a vector of Rf passed in for MAR both cover the same time period
            }
        }
        
        switch(method,
            full   = {len = length(R)},
            subset = {len = length(r)} #previously length(R)
        ) # end switch

	switch(stat,
	    risk = {result = sqrt(sum((r - MAR)^2/len))},
	    variance = {result = sum((r - MAR)^2/len)},
	    potential = {result = sum((r - MAR)/len)}
	    )
        return(result)
    }
    else {
        R = checkData(R)
        result = apply(R, MARGIN = 2, UpsideRisk, MAR = MAR, method = method, stat = stat, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("Upside Risk (MAR = ",MAR,"%)", sep="")
        return(result)
    }
}
