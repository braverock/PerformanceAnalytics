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
  VecSig = solve( diag( N^2 ) - expm(- TsT * tau ) ) %*% TsT %*% VecSig_tau
  Sig = VecSig
  dim( Sig ) = c( N , N )
}

