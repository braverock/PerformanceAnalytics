## A set of functions for Triple Penance Rule
##
## These set of functions are used for calculating Maximum Drawdown and Maximum Time under water
## for different distributions such as normal and non-normal.
## 
## FUNCTIONS:
## dd_norm
## tuw_norm
## get_minq
## getQ
## get_TuW
##
## REFERENCE:
## Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs
## and the ‘Triple Penance’ Rule(January 1, 2013).

dd_norm<-function(x,confidence){
    # DESCRIPTION:
    # A function to return the maximum drawdown for a normal distribution

    # Inputs:
    # R: The Return Series
    #
    # confidence: The confidence Level
    sd = StdDev(x)
    mu = mean(x, na.rm = TRUE)
    dd = max(0,((qnorm(1-confidence)*sd)^2)/(4*mu))
    t = ((qnorm(1-confidence)*sd)/(2*mu))^2

    return(c(dd*100,t))
}

tuw_norm<-function(x,confidence){
    # DESCRIPTION:
    # A function to return the Time under water

    # Inputs:
    # R: Return series
    # confidence: The confidence level
    sd = StdDev(x)
    mu = mean(x,na.rm = TRUE)
    tuw = ((qnorm(1-confidence)*sd)/mu)^2

    return(tuw)
}



get_minq<-function(R,confidence){
  
    # DESCRIPTION:
    # A function to get the maximum drawdown for first order serially autocorrelated
    # returns from the quantile function defined for accumulated returns for a 
    # particular confidence interval

    # Inputs:
    # R: The function takes Returns as the input
    #
    # confidence: The confidence interval.
    x = checkData(R)
    x = na.omit(x)
    mu = mean(x, na.rm = TRUE)
    sigma_infinity = StdDev(x)
    phi = cov(x[-1],x[-length(x)])/(cov(x[-length(x)]))

    sigma = sigma_infinity*((1-phi^2)^0.5)
    dp0 = 0
    q_value = 0
    bets = 0
   if(phi>=0 & mu >0){
        while(q_value <= 0){
        bets = bets + 1
        q_value = getQ(bets, phi, mu, sigma, dp0, confidence)
    }
        minQ = golden_section(0,bets,getQ,TRUE,phi,mu,sigma,dp0,confidence)
    }
    else{
    if(phi<0){
        warning(paste("NaN produced because phi < 0 ",colnames(x)))
    }
    if(mu<0){
        warning(paste("NaN produced because mu < 0 ",colnames(x)))

    }
        minQ = list(value=NaN,x=NaN)
    }
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

get_TuW<-function(R,confidence){

    # DESCRIPTION:
    # A function to generate the  time under water
    #
    # Inputs:
    # R: The function takes Returns as the input.
    #
    # confidence: Confidence level of the quantile function


    x = checkData(R)
    x = na.omit(x)
    mu = mean(x, na.rm = TRUE)
    sigma_infinity = StdDev(x)
    phi = cov(x[-1],x[-length(x)])/(cov(x[-length(x)]))
    sigma = sigma_infinity*((1-phi^2)^0.5)
    
    dp0 = 0
    q_value = 0
    bets = 0
    if(phi >=0 & mu >0){
    while(q_value <= 0){
        bets = bets + 1
        q_value = getQ(bets, phi, mu, sigma, dp0, confidence)
    }
        TuW = golden_section(bets-1,bets,diff_Q,TRUE,phi,mu,sigma,dp0,confidence)
    }
    else{
    if(phi<0){
        warning(paste("NaN produced because phi < 0 ",colnames(x)))
    }
    if(mu<0){
        warning(paste("NaN produced because mu < 0 ",colnames(x)))
    }
 
        TuW = list(x=NaN)
    }
    return(TuW$x)
}



diff_Q<-function(bets,phi,mu,sigma,dp0,confidence){
  
  # DESCRIPTION:
  # The functions to be minimized to calculate the maximum Time Under water using
  # Golden section algorithm
  #
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
  
    return(abs(getQ(bets,phi,mu,sigma,dp0,confidence)))
}

