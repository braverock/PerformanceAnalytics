#'@title Minimum Track Record Length
#'
#'@description
#'Minimum Track Record Length will tell us “How long should a track record be in 
#'order to have statistical confidence that its Sharpe ratio is above a given 
#'threshold? ". If a track record is shorter than MinTRL, we do not have enough
#'confidence that the observed Sharpe Ratio is above the designated threshold.
#'The reference Sharpe Ratio should be less than the observed Sharpe Ratio and 
#'the Values should be given in non-annualized terms, in the same periodicity as
#'the return series. The Minimum Track Record Length is also given in the same 
#'Periodicity as the Return Series.
#'
#'\deqn{MinTRL = n^\ast = 1+\biggl[1-\hat{\gamma_3}\hat{SR}+\frac{\hat{\gamma_4}}{4}\hat{SR^2}\biggr]\biggl(\frac{Z_\alpha}{\hat{SR}-SR^\ast}\biggr)^2}
#'
#'$\gamma{_3}$ and $\gamma{_4}$ are the skewness and kurtosis respectively. 
#'It is important to note that MinTRL is expressed in terms of number of observations,
#'not annual or calendar terms.
#'
#'@aliases MinTrackRecord
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset return 
#'@param Rf the risk free rate of return
#'@param refSR the reference Sharpe Ratio,in the same periodicity as the returns(non-annualized)
#'@param p the confidence level
#'@param weights the weights for the portfolio
#'@param sr Sharpe Ratio,in the same periodicity as the returns(non-annualized)
#'@param sk Skewness, in the same periodicity as the returns(non-annualized)
#'@param kr Kurtosis, in the same periodicity as the returns(non-annualized)
#'
#'@reference Bailey, David H. and Lopez de Prado, Marcos, \emph{The Sharpe Ratio 
#'Efficient Frontier} (July 1, 2012). Journal of Risk, Vol. 15, No. 2, Winter
#' 2012/13
#'@keywords ts multivariate distribution models
#'@examples
#'
#'data(edhec)
#'MinTrackRecord(edhec[,1],refSR=0.20)


MinTrackRecord<-function(R = NULL, refSR,Rf=0,p = 0.95, weights = NULL,sr = NULL,sk = NULL, kr = NULL, ...){
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
        if(is.null(sr) | is.null(sk) | is.null(kr)){
             stop("You must either pass R or the Sharpe ratio, Skewness, Kurtosis,n etc")
        }
    }
    #If weights are not taken into account a message is displayed
    #if(is.null(weights)){
     #   message("no weights passed,will calculate Minimum Track Record Length for each column")
    #}
   
    if(!is.null(dim(Rf))){
        Rf = checkData(Rf)
    }
    #If the refSR is greater than SR an error is displayed
    if(refSR>sr){
        stop("The Reference Sharpe Ratio should be less than the Observed Sharpe Ratio")
    }

    result = 1 + (1 - sk*sr + ((kr-1)/4)*sr^2)*(qnorm(p)/(sr-refSR))^2
    if(!is.null(dim(result))){ 
        colnames(result) = columnnames
        rownames(result) = paste("Minimum Track Record Length(p=",round(p*100,1),"%):")
    }
    return(result)
}

