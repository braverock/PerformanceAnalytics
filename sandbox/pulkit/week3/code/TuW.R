#' @title
#' Time Under Water
#'
#' @description
#' \code{TriplePenance} calculates the maximum 
#' Time under water for a particular confidence interval. 
#'
#'@param confidence The confidence level
#' @param R Hedge Fund log Returns
#' 
#' @reference Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).

TuW<-function(R,confidence,...){
    x = checkData(R)
    columns = ncol(R)
    i = 0 
    tp = matrix(nrow=columns)


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


get_TuW<-function(R,confidence){

    # DESCRIPTION:
    # A function to generate the  time under water
    #
    # Inputs:
    # R: The function takes Returns as the input.
    #
    # confidence: Confidence level of the quantile function


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
    TuW = golden_section(bets-1,bets,TRUE,diff,phi,mu,sigma,dp0,confidence)
    return(TuW$x)
}



diff<-function(bets,phi,mu,sigma,dp0,confidence){
    return(abs(getQ(bets,phi,mu,sigma,dp0,confidence)))
}



    for(i in 1:columns){
        column_TuW = get_TuW(x[,i],confidence)
        tp[i] <- column_TuW
    }


rownames(tp)<-colnames(R)
colnames(tp)<-"Max Time Under Water"
return(tp)
}
      

