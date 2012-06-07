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
        xl = min(X[ , n ] )
        xh = max(X[ , n ] )
        #x = [xl : (xh-xl)/100 : xh]
        
        # posterior numerical
        h3 = pHist(X[ ,n] , p_ , NBins )
        
        # posterior analytical        
        h4 = plot( x , normpdf( x , Mu_[n] , sqrt( Sigma_[n,n] ) ) )        
        
        # prior analytical
        h2 = plot( x , normpdf( x , Mu[n] ,sqrt( Sigma[n,n] ) ) )
        
        # xlim( cbind( xl , xh ) )
        # legend([h3 h4 h2],'numerical', 'analytical', 'prior')
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

