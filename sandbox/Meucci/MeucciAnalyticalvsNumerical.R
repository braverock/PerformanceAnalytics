# TODO: Determine how to extend correlation view to multiple assets
# TODO: Create plot distributions function

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


# This example script compares the numerical and the analytical solution of entropy-pooling, see
# "A. Meucci - Fully Flexible Views: Theory and Practice 
# and associated script S_Main
# Example compares analytical vs. numerical approach to entropy pooling

# Code by A. Meucci, September 2008
# Last version available at www.symmys.com > Teaching > MATLAB

###############################################################
# prior
###############################################################
library( matlab )
library( MASS )
# analytical representation
N = 2 # market dimension (2 assets)
Mu = zeros( N , 1 )
r= .6
Sigma = ( 1 - r ) * eye( N ) + r * ones( N , N ) # nxn correlation matrix with correlaiton 'r' in off-diagonals

# numerical representation
J = 100000 # number of scenarios
p = ones( J , 1 ) / J
dd = mvrnorm( J / 2 , zeros( N , 1 ) , Sigma ) # distribution centered on (0,0) with variance Sigma
X = ones( J , 1 ) %*% t(Mu) + rbind( dd , -dd ) # JxN matrix of scenarios

###############################################################
# views
###############################################################

# location
Q = matrix( c( 1 , -1 ) , nrow = 1 ) # long the first and asset and short the second asset produces an expectation (of Mu_Q calculated below)
Mu_Q = .5

# scatter
G = matrix( c( -1 , 1 ) , nrow = 1 )
Sigma_G = .5^2

###############################################################
#  posterior 
###############################################################

# analytical posterior
RevisedMuSigma = Prior2Posterior( Mu , Q , Mu_Q , Sigma , G , Sigma_G )
Mu_ = RevisedMuSigma$M_

# numerical posterior
Aeq = ones( 1 , J )  # constrain probabilities to sum to one...
beq = 1

# create views
    QX = X %*% t(Q) # a Jx1 matrix

    Aeq = rbind( Aeq , t(QX) )    # ...constrain the first moments... 
        # QX is a linear combination of vector Q and the scenarios X
    
    beq = rbind( beq , Mu_Q )
    
    SecMom = G %*% Mu_ %*% t(Mu_) %*% t(G) + Sigma_G  # ...constrain the second moments... 
        # We use Mu_ from analytical result. We do not use Revised Sigma because we are testing whether
        # the numerical approach for handling expectations of covariance matches the analytical approach
        # TODO: Can we perform this procedure without relying on Mu_ from the analytical result?
    GX = X %*% t(G)
    
    for ( k in 1:nrow( G ) )
        {
        for ( l in k:nrow( G ) )
            {
            Aeq = rbind( Aeq , t(GX[ , k ] * GX[ , l ] ) )
            beq = rbind( beq , SecMom[ k , l ] )
            }
        }

emptyMatrix = matrix( , nrow = 0 , ncol = 0 )
p_ = EntropyProg( p , emptyMatrix , emptyMatrix , Aeq , beq ) # ...compute posterior probabilities

###############################################################
# plots
###############################################################
PlotDistributions( X , p , Mu , Sigma , p_ , Mu_ , Sigma_ )