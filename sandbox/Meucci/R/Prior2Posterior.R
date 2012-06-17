# TODO: Determine how to extend correlation view to multiple assets
# TODO: Create plot distributions function

#' 
#' @param X 
#' @param p 
#' @param Mu 
#' @param Sigma 
#' @param p_ 
#' @param Mu_ 
#' @param Sigma_ 
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
    #h3 = pHist(X[ ,n] , p_ , NBins )
        
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


#' Calculate the full-confidence posterior distributions of Mu and Sigma
#'
#' @param M     a numeric vector with the Mu of the normal reference model
#' @param Q     a numeric vector used to construct a view on expectation of the linear combination Q %*% X
#' @param M_Q   a numeric vector with the view of the expectations of QX
#' @param S     a covariance matrix for the normal reference model
#' @param G     a numeric vector used to construct a view on covariance of the linear combination G %*% X
#' @param S_G   a numeric with the expectation associated with the covariance of the linear combination GX
#'
#' @return a list with 
#'             M_   a numeric vector with the full-confidence posterior distribution of Mu
#'             S_   a covariance matrix with the full-confidence posterior distribution of Sigma
#'
#' @references 
#' \url{http://www.symmys.com}
#' See Meucci script Prior2Posterior.m attached to Entropy Pooling Paper
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
Prior2Posterior = function( M , Q , M_Q , S , G , S_G )
{  
  # See Appendix A.1 formula 49 for derivation
  M_ = M + S %*% t(Q) %*% solve( Q %*% S %*% t(Q) ) %*% (M_Q - Q %*% M)
    
  # See Appendix A.1 formula 57 for derivation
  S_= S + (S %*% t(G)) %*% ( solve( G %*% S %*% t(G) ) %*% S_G %*% solve( G %*% S %*% t(G) ) - solve( G%*%S%*%t(G) ) ) %*% ( G %*% S )
    
  return ( list( M_ = M_ , S_ = S_ ) )    
}

pHist = function( X, p, nBins )
{
  bins = seq(from = min(X), to = max(X), by = (max(X) - min(X))/nBins)
  histObject1 <- hist( X[,1], breaks = bins, plot = FALSE )
  histObject2 <- hist( X[,2], breaks = bins, plot = FALSE )
  x = as.matrix(histObject1$mids)
  n = cbind( histObject1$counts, histObject2$counts )
  D = x[2,] - x[1,]
  np = zeros( length(x), 1 )
  for( s in 1:length(x) ) {
    pVector = NULL
    for( i in 1:nrow(X) ) {
      if( ( X[i,1] >= (x[s,] - D/2) & X[i,1] <= (x[s,] + D/2) ) | ( X[i,2] >= (x[s,] - D/2) & X[i,2] <= (x[s,] + D/2) ) ) {
        pVector = rbind(pVector, p[i,] )
      }    
    }
    if( length(pVector) != 0 ) { np[s,] = sum( pVector ) }
    f = np/D
  }
}