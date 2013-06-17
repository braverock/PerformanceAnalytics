#'@title Probabilistic Sharpe Ratio
#'@description
#'Given a predefined
#'benchmark4 Sharpe ratio (), the observed Sharpe  Ratiô can be expressed
#' in probabilistic
#'
#'@param R the return series
#'@param Rf the risk free rate of return
#'@param refSR the reference Sharpe Ratio
#'@param the confidence level
#'@param weights the weights for the portfolio

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
    if(is.null(weights)){
        message("no weights passed,will calculate Probability Sharpe Ratio for each column")
    }
   
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)
    result = 1 + (1 - sk*sr + ((kr-1)/4)*sr^2)*(qnorm(p)/(sr-refSR))^2
    if(!is.null(dim(result))){ 
        colnames(result) = columnnames
        rownames(result) = paste("Minimum Track Record Length(p=",round(p*100,1),"%):")
    }
    return(result)
}

