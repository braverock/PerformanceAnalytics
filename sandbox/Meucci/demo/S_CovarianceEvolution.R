# This script represents the evolution of the covariance of an OU process in terms of the dispersion ellipsoid
# see A. Meucci (2009) 
# "Review of Statistical Arbitrage, Cointegration, and Multivariate Ornstein-Uhlenbeck"
# available at ssrn.com

# Code by A. Meucci, April 2009
# Most recent version available at www.symmys.com > Teaching > MATLAB

# input parameters of multivariate OU process
K = 1
J = 1

x0 = matrix( runif(K + 2*J), ncol = 1 )

Mu = matrix( runif(K + 2*J), ncol = 1 )

A = matrix( runif( ( K + 2*J )^2 ), nrow = K + 2*J, byrow = T ) - .5
ls = matrix( runif( K ), ncol = 1 ) - .5
gs = matrix( runif( J ), ncol = 1 ) - .5
os = matrix( runif( J ), ncol = 1 ) - .5

S = matrix( runif( ( K + 2*J )^2 ), nrow = K + 2*J, byrow = T ) - .5

ts_0 = .01 * as.matrix( seq( from = 0, to = 100, by = 10 ) )
NumSimul = 10000

# process inputs
Gamma = diag( ls )
for ( j in 1 : J )
{
  G = rbind( cbind( gs[j], os[j] ), cbind( -os[j], gs[j] ) )
  Gamma = as.matrix( bdiag( Gamma , G ) )
}

Theta = A %*% Gamma %*% solve( A )

# one-step exact simulation
Sigma = S %*% t( S )
X = t( x0 )
mx = dim( X )[ 1 ]
nx = dim( X )[ 2 ]
X_0 = matrix( t ( matrix( X , mx , nx ) ), mx * NumSimul , nx , byrow=T )
OUstepResult = OUstep( X_0 , ts_0[ nrow( ts_0 ) ] , Mu , Theta , Sigma )

# multi-step simulation: exact and Euler approximation
X_t = X_0
X_tE = X_t
for ( s in 1:length( ts_0 ) )
{
  Dt = ts_0[ 1 ]
  if ( s > 1 )
  {
    Dt = ts_0[s] - ts_0[ s - 1 ]
  }
  PostOUStepResult = OUstep( X_t , Dt , Mu , Theta , Sigma )
  #[X_tE,MuHat_tE,SigmaHat_tE]=OUstepEuler(X_tE,Dt,Mu,Theta,Sigma);
}
           
# plots  
Pick = cbind( K + 2*J - 1, K + 2*J )
           
# horizon simulations
plot( OUstepResult$X_t[ , Pick[ 1 ] ] , OUstepResult$X_t[ , Pick[ 2 ] ] )
           
# horizon location
plot( OUstepResult$Mu_t[ Pick[ 1 ] ] , OUstepResult$Mu_t[ Pick[ 2 ] ] )
           
# horizon dispersion ellipsoid 
# TwoDimEllipsoid(MuHat_t1(Pick),SigmaHat_t1(Pick,Pick),2,0,0);
           
# starting point
plot( x0[ Pick[ 1 ] ] , x0[ Pick[ 2 ] ] )
           
# starting generating dispersion ellipsoid
# TwoDimEllipsoid(x0(Pick),Sigma(Pick,Pick),2,0,0);