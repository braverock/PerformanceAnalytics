#' Plot numerical and analytical prior and posterior distributions
#'
#' @param X       a vector containing the dataset
#' @param p       a vector cotaining the prior probability values
#' @param Mu      a vector containing the prior means
#' @param Sigma   a vector containing the prior standard deviations
#' @param p_      a vector containing the posterior probability values
#' @param Mu_     a vector containing the posterior means
#' @param Sigma_  a vector containing the posterior standard deviations
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
PlotDistributions = function( X , p , Mu , Sigma , p_ , Mu_ , Sigma_ )
{
  J = nrow( X )
  N = ncol( X )
    
  NBins = round( 10*log( J ) )
    
  for ( n in 1:N )
  {        
    # set ranges
    xl = min( X[ , n ] )
    xh = max( X[ , n ] )
    x = as.matrix(seq(from=xl, to=xh, by=(xh-xl)/100))
        
    # posterior numerical
    # h3 = pHist(X[ ,n] , p_ , NBins )
        
    # posterior analytical        
    y1 = dnorm( x , Mu_[n] , sqrt( Sigma_[n,n] ) )
    h4 = plot( x , y1,  type='l', col='red', xlab='', ylab='' )        
        
    # prior analytical
    par(new = TRUE)
    y2 = dnorm( x , Mu[n] ,sqrt( Sigma[n,n] ) )
    h2 = plot( x , y2, type='l', col='blue', xlab='', ylab='' )
        
    # xlim( cbind( xl , xh ) )
    legend(x = 1.5, y =0.4 ,legend=c("analytical","prior"), lwd=c(0.2,0.2), lty=c(1,1), col=c("red", "blue"))
  }
}