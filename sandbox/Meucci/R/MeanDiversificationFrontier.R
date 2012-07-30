GenFirstEigVect = function( S , A )
{
  N = nrow( S )
  P = diag( N )

  if ( qr( A )$rank > 0 )
  {
    P = diag(N) - t( A ) %*% solve( A  %*% t( A ) ) %*% A
  }

  tmp = eigen( P %*% S %*% t(P) )
  E_ = tmp$vectors
  L_ = tmp$values
  
  I = which.max(  L_ )
  e = E_[,I]
  
  return( e )
}

GenPCBasis = function( S , A )
{  
  if ( length( A ) == 0 )
  {
    N = nrow( S )
    K = 0
    tmp = eigen( S )
    E_ = tmp$vectors
    E = E_
    for ( n in 1:N )
    {
      E[ , n ] = E_[ , N - n + 1 ]
      L[ n ] = L_[ N - n + 1 , N - n + 1 ]
    }
  }
}