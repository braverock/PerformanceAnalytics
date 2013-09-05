#' @title Probabilistic Sharpe Ratio
#'
#' @description
#' Given a predefined benchmark Sharpe ratio ,the observed Sharpe Ratio 
#' can be expressed in probabilistic terms known as the Probabilistic 
#' Sharpe Ratio PSR takes higher moments  into account and delivers a
#' corrected, atemporal measure of performance expressed in terms of 
#' probability of skill. The reference Sharpe Ratio should be less than 
#' the Observed Sharpe Ratio.
#' 
#' \deqn{\hat{PSR}(SR^{*}) = Z\bigg[\frac{(\hat{SR}-SR^{*})\sqrt{n-1}}{\sqrt{1-\hat{\gamma_3}SR^{*} + \frac{\hat{\gamma_4}-1}{4}\hat{SR^2}}}\bigg]}

#' Here \eqn{n} is the track record length or the number of data points. It can be daily,weekly or yearly depending on the input given

#' \eqn{\hat{\gamma{_3}}} and \eqn{\hat{\gamma{_4}}} are the skewness and kurtosis respectively.
#'
#'
#' @aliases ProbSharpeRatio
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset return 
#' @param Rf the risk free rate of return
#' @param refSR the reference Sharpe Ratio, can be a single value or a vector for a multicolumn
#'  return series.Should be non-annualized , in the same periodicity as the returns.
#' @param p the confidence level
#' @param weights the weights for the portfolio
#' @param sr Sharpe Ratio, in the same periodicity as the returns(non-annualized).
#' To be given in case the return series is not given.
#' @param sk Skewness, in the same periodicity as the returns(non-annualized).
#' To be given in case the return series is not given.
#' @param kr Kurtosis, in the same periodicity as the returns(non-annualized).
#' To be given in case the return series is not given.
#' @param n track record length. To be given in case the return series is not given.
#' @param \dots any other passthru variable 
#'@author Pulkit Mehrotra
#'@seealso \code{\link{PsrPortfolio}} \code{\link{table.PSR}} \code{\link{MinTrackRecord}}
#' @references Bailey, David H. and Lopez de Prado, Marcos, \emph{The Sharpe Ratio 
#' Efficient Frontier} (July 1, 2012). Journal of Risk, Vol. 15, No. 2, Winter
#' 2012/13
#'
#' @keywords ts multivariate distribution models
#'
#' @examples
#'
#' data(edhec)
#' ProbSharpeRatio(edhec[,1],refSR = 0.23) 
#' ProbSharpeRatio(refSR = 1/12^0.5,Rf = 0,p=0.95,sr = 2/12^0.5,sk=-0.72,kr=5.78,n=59)
#' ProbSharpeRatio(edhec[,1:2],refSR = c(0.28,0.24)) 
#'@export

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
        if(length(refSR)==1){
          refSR = rep(refSR,columns)
        }
        if(length(refSR)!=columns){
          stop("Reference Sharpe Ratio should be given for each series")
        }
        
 
    }
    # If R is passed as null checking for sharpe ratio , skewness and kurtosis 
    else{

        if(is.null(sr) | is.null(sk) | is.null(kr) | is.null(n)){
             stop("You must either pass R or the Sharpe ratio, Skewness, Kurtosis,n etc")
       }
    }
   
    if(!is.null(dim(Rf))){
        Rf = checkData(Rf)
    }
    #If the Reference Sharpe Ratio is greater than the Observred Sharpe Ratio an error is displayed
        index = which(refSR>sr)
    if(length(index)!=0){
        if(length(index)==columns){
            stop("The reference Sharpe Ratio greater than the Observed Sharpe ratio for all the cases")
        }
        sr = sr[-index]
        refSR = refSR[-index]
        sk = sk[-index]
        kr = kr[-index]
        warning(paste("The Reference Sharpe Ratio greater than the Observed Sharpe Ratio for case",columnnames[index],"\n"))
        
    }
    result = pnorm(((sr - refSR)*(n-1)^(0.5))/(1-sr*sk+sr^2*(kr-1)/4)^(0.5))
    columnnames = columnnames[-index]
    if(!is.null(dim(result))){ 
        colnames(result) = paste(columnnames,"(SR >",refSR,")") 
        
        rownames(result) = paste("Probabilistic Sharpe Ratio(p=",round(p*100,1),"%):")
    }
    return(result)
    
}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: ProbSharpeRatio.R $
#
##############################################################################
