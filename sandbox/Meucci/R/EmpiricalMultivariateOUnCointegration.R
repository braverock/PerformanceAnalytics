FitOU = function ( Y, tau )
{
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
  VecSig = solve( diag( N^2 ) - expm( Matrix( -TsT * tau ) ) ) %*% TsT %*% VecSig_tau
  Sig = VecSig
  dim( Sig ) = c( N , N )
  
  return( list( Mu = Mu, Th = Th, Sig = Sig ) )
}

OUstep = function( X_0 , t , Mu , Th , Sig )
{
  NumSimul = nrow( X_0 )
  N = ncol( X_0 )

  # location
  ExpM = expm( Matrix( -Th %*% t ) )
  
  # repmat = function(X,m,n) - R equivalent of repmat (matlab)
  X = t( Mu - ExpM %*% Mu )
  mx = dim( X )[1]
  nx = dim( X )[2]
  Mu_t = matrix( t ( matrix( X , mx , nx*1 ) ), mx * NumSimul, nx * 1, byrow = T ) + t( X_0 %*% ExpM )
              
  # scatter
  TsT = kronecker( Th , diag( N ) ) + kronecker( diag( N ) , Th )
              
  VecSig = Sig
  dim( VecSig ) = c( N^2 , 1 )
  VecSig_t = solve( TsT ) %*% ( diag( N^2 ) - expm( Matrix( -TsT %*% t ) ) ) %*% VecSig
  Sig_t = VecSig_t
  dim( Sig_t ) = c( N , N )
  Sig_t = ( Sig_t + t( Sig_t ) ) / 2

  Eps = mvrnorm( NumSimul, rep( 0 , N ), Sig_t )
                     
  X_t = Mu_t + Eps
  Mu_t = t( colMeans( Mu_t ) )
  
  return( list( X_t = X_t, Mu_t = Mu_t, Sig_t = Sig_t ) )
}

ProjectOU = function( x_0 , t , Mu , Th , Sig )
{
  N = length( x_0 )

  # location
  Mu_t = Mu + expm( Matrix( -Th %*% t ) ) %*% ( x_0 - Mu )

  # scatter
  TsT = kronecker( Th , diag( N ) ) + kronecker( diag( N ) , Th )

  VecSig = Sig
  dim( VecSig ) = c( N^2 , 1 )
  VecSig_t = solve( TsT ) %*% ( diag( N^2 ) - expm( Matrix( -TsT %*% t ) ) ) %*% VecSig
  Sig_t = VecSig_t
  dim( Sig_t ) = c( N , N )
}