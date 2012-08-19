#' Generate the next element based on Ornstein-Uhlenbeck Process
#' 
#' @param X_0   a matrix containing the starting value of each process
#' @param t     a numeric containing the timestep   
#' @param Mu    a vector containing the unconditional expectation of the process
#' @param Th    a transition matrix, i.e., a fully generic square matrix that steers the deterministic portion
#'              of the evolution of the process
#' @param Sig   a square matrix that drives the dispersion of the process
#'
#' @return        a list containing
#' @return X_t    a vector containing the value of the process after the given timestep
#' @return Mu_t   a vector containing the conditional expectation of the process
#' @return Sig_t  a matrix containing the covariance after the time step
#'
#' \deqn{ X_{t+ \tau } =  \big(I- e^{- \theta  \tau } \big)  \mu  +  e^{- \theta  \tau } X_{t} +   \epsilon _{t, \tau } }
#' @references 
#' A. Meucci - "Review of Statistical Arbitrage, Cointegration, and Multivariate Ornstein-Uhlenbeck" - Formula (2)
#' \url{http://ssrn.com/abstract=1404905}
#' @author Manan Shah \email{mkshah@@cmu.edu}
OUstep = function( X_0 , t , Mu , Th , Sig )
{
  NumSimul = nrow( X_0 )
  N = ncol( X_0 )
  
  # location
  ExpM = expm( -Th * t )
  
  # repmat = function(X,m,n) - R equivalent of repmat (matlab)
  X = t( Mu - ExpM %*% Mu )
  mx = dim( X )[1]
  nx = dim( X )[2]
  Mu_t = matrix( t ( matrix( X , mx , nx*1 ) ), mx * NumSimul, nx * 1, byrow = T ) + X_0 %*% ExpM
  
  # scatter
  TsT = kronecker( Th , diag( N ) ) + kronecker( diag( N ) , Th )
  
  VecSig = Sig
  dim( VecSig ) = c( N^2 , 1 )
  VecSig_t = solve( TsT ) %*% ( diag( N^2 ) - expm( -TsT * t ) ) %*% VecSig
  Sig_t = VecSig_t
  dim( Sig_t ) = c( N , N )
  Sig_t = ( Sig_t + t( Sig_t ) ) / 2
  
  Eps = mvrnorm( NumSimul, rep( 0 , N ), Sig_t )
  
  X_t = Mu_t + Eps
  Mu_t = t( colMeans( Mu_t ) )
  
  return( list( X_t = X_t, Mu_t = Mu_t, Sig_t = Sig_t ) )
}

#' Generate the next element based on Ornstein-Uhlenbeck process using antithetic concept and assuming that the
#' Brownian motion has Euler discretization
#' 
#' @param X_0   a matrix containing the starting value of each process
#' @param t     a numeric containing the timestep   
#' @param Mu    a vector containing the unconditional expectation of the process
#' @param Th    a transition matrix, i.e., a fully generic square matrix that steers the deterministic portion
#'              of the evolution of the process
#' @param Sig   a square matrix that drives the dispersion of the process
#'
#' @return        a list containing
#' @return X_t    a vector containing the value of the process after the given timestep
#' @return Mu_t   a vector containing the conditional expectation of the process
#' @return Sig_t  a matrix containing the covariance after the time step
#'
#' \deqn{ X_{t+ \tau } =  \big(I- e^{- \theta  \tau } \big)  \mu  +  e^{- \theta  \tau } X_{t} +   \epsilon _{t, \tau } }
#' @references 
#' A. Meucci - "Review of Statistical Arbitrage, Cointegration, and Multivariate Ornstein-Uhlenbeck" - Formula (2)
#' \url{http://ssrn.com/abstract=1404905}
#' @author Manan Shah \email{mkshah@@cmu.edu}
OUstepEuler = function( X_0 , Dt , Mu , Th , Sig )
{
  NumSimul = nrow( X_0 )
  N = ncol( X_0 )
  
  # location
  ExpM = expm( as.matrix( -Th %*% Dt ) )
  
  # repmat = function(X,m,n) - R equivalent of repmat (matlab)
  X = t( Mu - ExpM %*% Mu )
  mx = dim( X )[1]
  nx = dim( X )[2]
  Mu_t = matrix( t ( matrix( X , mx , nx*1 ) ), mx * NumSimul, nx * 1, byrow = T ) + X_0 %*% ExpM
  
  # scatter
  Sig_t = Sig %*% Dt
  Eps = mvrnorm( NumSimul / 2, rep( 0 , N ) , Sig_t )
  Eps = rbind( Eps, -Eps)
  
  X_t = Mu_t + Eps
  Mu_t = t( colMeans( X_t ) )
  
  return( list( X_t = X_t, Mu_t = Mu_t, Sig_t = Sig_t ) )
}

#' Fit the Ornstein-uhlenbeck process to model the behavior for different values of the timestep.
#' 
#' @param Y       a matrix containing the value of a process at various time steps.
#' @param tau     a numeric containing the timestep   
#'
#' @return        a list containing
#' @return Mu     a vector containing the expectation of the process
#' @return Sig    a matrix containing the covariance of the resulting fitted OU process
#' @return Th     a transition matrix required for defining the fitted OU process
#'
#' \deqn{  x_{t+ \tau } =  \big(I- e^{- \theta  \tau } \big)  \mu  +  e^{- \theta  \tau } x_{t},
#' vec \big(  \Sigma _{ \tau } \big)  \equiv    \big( \Theta  \oplus  \Theta \big) ^{-1}  \big(I- e^{( \Theta   \oplus   \Theta ) \tau } \big)  vec \big(  \Sigma \big) }
#' @references 
#' A. Meucci - "Review of Statistical Arbitrage, Cointegration, and Multivariate Ornstein-Uhlenbeck" - Formula (8),(9)
#' \url{http://ssrn.com/abstract=1404905}
#' @author Manan Shah \email{mkshah@@cmu.edu}
FitOU = function ( Y, tau )
{
  library(expm)
  T = nrow( Y )
  N = ncol( Y )
  
  X = Y[ -1 , ]
  F = cbind( rep( 1, T-1 ), Y [ 1:T-1 ,] )
  E_XF = t( X ) %*% F / T
  E_FF = t( F ) %*% F / T
  B = E_XF %*% solve( E_FF )
  
  Th = -logm ( B [ , -1 ] ) / tau
  Mu = solve( diag( N ) - B[ , -1 ] ) %*% B[ , 1 ]
  
  U = F %*% t( B ) - X
  Sig_tau = cov( U )
  
  N = length( Mu )
  TsT = kronecker( Th , diag( N ) ) + kronecker( diag( N ) , Th )
  
  VecSig_tau = Sig_tau
  dim( VecSig_tau ) = c( N^2 , 1 )
  VecSig = solve( diag( N^2 ) - expm( as.matrix( -TsT * tau ) ) ) %*% TsT %*% VecSig_tau
  Sig = VecSig
  dim( Sig ) = c( N , N )
  
  return( list( Mu = Mu, Th = Th, Sig = Sig ) )
}