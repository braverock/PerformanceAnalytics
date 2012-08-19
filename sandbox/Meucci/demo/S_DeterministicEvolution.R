# This script animates the evolution of the determinstic component of an OU process 
# see A. Meucci (2009) 
# "Review of Statistical Arbitrage, Cointegration, and Multivariate Ornstein-Uhlenbeck"
# available at ssrn.com

# Code by A. Meucci, April 2009
# Most recent version available at www.symmys.com > Teaching > MATLAB

# input parameters of multivariate OU process
K = 1
J = 1

x0 = rep( 1, K + 2*J )

Mu = 0 * matrix( runif(K + 2*J), ncol = 1 )

A = rbind( c(1,0,0), c(0,1,0), c(0,0,1) )

ls = -10
gs = 10
os = 100

ts_0 = .001 * as.matrix( seq( from = 0, to = 300, by = 1 ) )

# process inputs
Gamma = diag( ls, nrow = length( ls ) )
for( j in 1:J )
{
  G = rbind( cbind( gs[j], os[j] ), cbind( -os[j], gs[j] ) )
  Gamma = as.matrix( bdiag( Gamma , G ) )
}

Theta = A %*% Gamma %*% solve( A )

# process dynamics 
S = 0 * matrix( runif( ( K + 2*J )^2 ), nrow = K + 2*J, byrow = T )
Sigma = S %*% t( S )
X_t = matrix(0, nrow = nrow( Theta ), ncol = length( ts_0 ) )
for( j in 1 : length( ts_0 ) )
{
  t = ts_0[ j ]
  X_t[ , j ] = OUstep( t( x0 ) , t , Mu , Theta , Sigma )$X_t
}

# plots
plot( ts_0 , X_t[ 1 , ] )
plot( ts_0 , X_t[ 2 , ] )
plot( ts_0 , X_t[ 3 , ] )

# t = 0:pi/50:10*pi;
# AnimateTrajectory(X_t(1,:),X_t(2,:),X_t(3,:))
