library(PerformanceAnalytics)
data(edhec)
#' @title
#' Triple Penance Rule
#'
#' @description
#' \code{TriplePenance} calculates the Maximum drawdown and the maximum 
#' Time under water for a particular confidence interval. These concepts 
#' are intenately related through the "triple penance" rule which states 
#' that under standard portfolio theory assumptions, it takes three times
#' longer to recover from the expected maximum drawdown than the time it 
#' takes to produce it, with the same confidence level. The framework is
#' generalized to deal with the case of first-order auto-correlated cashflows
#'
#' @param R Hedge Fund log Returns
#' 
#' @reference Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).

MaxDD<-function(R,confidence,...)
{
    x = checkData(R)
    columns = ncol(x)
    i = 0 
    tp = matrix(nrow=columns,ncol=2)
get_minq<-function(R,confidence){
  
    # DESCRIPTION:
    # A function to get the maximum drawdown for first order serially autocorrelated
    # returns from the quantile function defined for accumulated returns for a 
    # particular confidence interval

    # Inputs:
    # R: The function takes Returns as the input
    #
    # confidence: The confidence interval of the input.
    x = checkData(R)
    mu = mean(x, na.rm = TRUE)
    sigma_infinity = StdDev(x)
    phi = cov(x[-1],x[-length(x)])/(cov(x[-length(x)]))
    sigma = sigma_infinity*((1-phi^2)^0.5)
    dp0 = 0
    q_value = 0
    bets = 0
    while(q_value <= 0){
        bets = bets + 1
        q_value = getQ(bets, phi, mu, sigma, dp0, confidence)
    }
    minQ = golden_section(0,bets,TRUE,getQ,phi,mu,sigma,dp0,confidence)
    return(c(-minQ$value*100,minQ$x))
}


getQ<-function(bets,phi,mu,sigma,dp0,confidence){

    # DESCRIPTION:
    # A function to get the quantile function for cumulative returns
    # and a  particular confidence interval.
    
    # Inputs:
    # bets: The number fo steps
    #
    # phi: The coefficient for AR[1]
    #
    # mu: The mean of the returns
    #
    # sigma: The standard deviation of the returns
    #
    # dp0: The r0 or the first return
    #
    # confidence: The confidence level of the quantile function
    mu_new = (phi^(bets+1)-phi)/(1-phi)*(dp0-mu)+mu*bets
    var = sigma^2/(phi-1)^2
    var = var*((phi^(2*(bets+1))-1)/(phi^2-1)-2*(phi^(bets+1)-1)/(phi-1)+bets +1)
    q_value = mu_new + qnorm(1-confidence)*(var^0.5)
    return(q_value)
}


     for(i in 1:columns){
        column_MinQ <- get_minq(x[,i],confidence)
        tp[i,] <- column_MinQ
    } 
 row.names(tp)<-colnames(R)
  colnames(tp) = c("MaxDD(in %)","t*")
  return(tp)
  
}


