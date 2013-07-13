#' @title Probabilistic Sharpe Ratio
#'
#' @description
#' Given a predefined benchmark Sharpe ratio ,the observed Sharpe Ratio 
#' can be expressed in probabilistic terms known as the Probabilistic 
#' Sharpe Ratio PSR takes higher moments  into account and delivers a
#' corrected, atemporal measure of performance expressed in terms of 
#' probability of skill. The reference Sharpe Ratio should be less than 
#' the Observed Sharpe Ratio.
#' \deqn{\hat{PSR}(SR^\ast) = Z\biggl[\frac{(\hat{SR}-SR^\ast)\sqrt{n-1}}{\sqrt{1-\hat{\gamma_3}SR^\ast + \frac{\hat{\gamma_4}-1}{4}\hat{SR^2}}}\biggr]}

#' Here $n$ is the track record length or the number of data points. It can be daily,weekly or yearly depending on the input given

#' $\hat{\gamma{_3}}$ and $\hat{\gamma{_4}}$ are the skewness and kurtosis respectively.

#'
#' @aliases ProbSharpeRatio
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset return 
#' @param Rf the risk free rate of return
#' @param refSR the reference Sharpe Ratio, in the same periodicity as the returns(non-annualized)
#' @param the confidence level
#' @param weights the weights for the portfolio
#' @param sr Sharpe Ratio, in the same periodicity as the returns(non-annualized)
#' @param sk Skewness, in the same periodicity as the returns(non-annualized)
#' @param kr Kurtosis, in the same periodicity as the returns(non-annualized)
#'
#' @references Bailey, David H. and Lopez de Prado, Marcos, \emph{The Sharpe Ratio 
#' Efficient Frontier} (July 1, 2012). Journal of Risk, Vol. 15, No. 2, Winter
#' 2012/13
#'
#' @keywords ts multivariate distribution models
#'
#' @examples
#'
#' data(edhec)
#' ProbSharpeRatio(edhec[,1],refSR = 0.28) 
#' ProbSharpeRatio(edhec,reSR = 0.28,Rf = 0.06)


ProbSharpeRatio<-
function(R = NULL, refSR,Rf=0,p = 0.95, weights = NULL,n = NULL,sr = NULL,sk = NULL, kr = NULL, ...){
    columns = 1
    columnnames = NULL
    #Error handling if R is not NULL
    if(!is.null(R)){
        x = checkData(R)
        columns = ncol(x)
        n = nrow(x)
        #Checking if the weights are provided or not
        if(!is.null(weights)){
            if(length(weights)!=columns){
                stop("number of items in weights is not equal to the number of columns in R")
            }
            else{
                # A potfolio is constructed by applying the weights
                x = Return.portfolio(R,weights)
                sr = SharpeRatio(x, Rf, p, "StdDev")
                sk = skewness(x)
                kr = kurtosis(x)
            }
        }
        else{
            sr = SharpeRatio(x, Rf, p, "StdDev")
            sk = skewness(x)
            kr = kurtosis(x)
        }

    columnnames = colnames(x)
 
    }
    # If R is passed as null checking for sharpe ratio , skewness and kurtosis 
    else{

        if(is.null(sr) | is.null(sk) | is.null(kr) | is.null(n)){
             stop("You must either pass R or the Sharpe ratio, Skewness, Kurtosis,n etc")
       }
    }
    #If weights are not taken into account a message is displayed
#    if(is.null(weights)){
 #       message("no weights passed,will calculate Probability Sharpe Ratio for each column")
  #  }
   
    if(!is.null(dim(Rf))){
        Rf = checkData(Rf)
    }
    #If the Reference Sharpe Ratio is greater than the Observred Sharpe Ratio an error is displayed
    if(refSR>sr){
        stop("The Reference Sharpe Ratio should be less than the Observed Sharpe Ratio")
    }
    result = pnorm(((sr - refSR)*(n-1)^(0.5))/(1-sr*sk+sr^2*(kr-1)/4)^(0.5))
    if(!is.null(dim(result))){ 
        colnames(result) = columnnames
        rownames(result) = paste("Probabilistic Sharpe Ratio(p=",round(p*100,1),"%):")
    }
    return(result)
    
}
