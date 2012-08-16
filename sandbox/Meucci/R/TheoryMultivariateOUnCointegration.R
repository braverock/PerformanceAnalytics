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

OUstepEuler = function( X_0 , Dt , Mu , Th , Sig )
{
  NumSimul = nrow( X_0 )
  N = ncol( X_0 )

  # location
  ExpM = expm( Matrix( -Th %*% Dt ) )

  # repmat = function(X,m,n) - R equivalent of repmat (matlab)
  X = t( Mu - ExpM %*% Mu )
  mx = dim( X )[1]
  nx = dim( X )[2]
  Mu_t = matrix( t ( matrix( X , mx , nx*1 ) ), mx * NumSimul, nx * 1, byrow = T ) + t( X_0 %*% ExpM )
              
  # scatter
  Sig_t = Sig %*% Dt
  Eps = mvrnorm( NumSimul / 2, rep( 0 , N ) , Sig_t )
  Eps = rbind( Eps, -Eps)
              
  X_t = Mu_t + Eps
  Mu_t = t( colMeans( X_t ) )
}