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
  return( list( E = E, L = L, G = G ) )
}

MaxEntropy = function( G , w_b , w_0 , Constr )
{
  # Nested function that computes fitness
  nestedfun = function( x )
  {
    v_ = G %*% ( x - w_b )
    p = v_ * v_
    R_2 = max( 10^(-10), p / colSums( p ) )
    Minus_Ent = t( R_2 ) * log( R_2 )
    return( Minus_Ent )
  }
  x = fmincon( @nestedfun , w_0 , Constr.A , Constr.b , Constr.Aeq , Constr.beq )
  return( x )
}

MeanTCEntropyFrontier = function( S , Mu , w_b , w_0 , Constr )
{
  # compute conditional principal portfolios
  GenPCBasisResult = GenPCBasis( S, emptyMatrix )

  # compute frontier extrema
  w_MaxExp = linprog( -Mu , Constr.A , Constr.b , Constr.Aeq , Constr.beq )
  MaxExp = t( Mu ) %*% ( w_MaxExp - w_b )

  w_MaxNe = MaxEntropy( G , w_b , w_0 , Constr )
  ExpMaxNe = t( Mu ) %*% ( w_MaxNe - w_b )

  # slice efficient frontier in NumPortf equally thick horizontal sections
  NumPortf = 10
  Grid_L = .0
  Grid_H = .9
  Grid = c( seq( from = Grid_L, to = Grid_H, length.out = NumPortf ) )
  TargetExp = ExpMaxNe + Grid %*% ( MaxExp - ExpMaxNe )

  # compute diversification distribution
  Weights = emptyMatrix
  R_2_s = emptyMatrix
  Ne_s = emptyMatrix
  m_s = emptyMatrix
  s_s = emptyMatrix

  for ( k in 1:NumPortf )
  {
    ConstR = Constr
    ConstR.Aeq = cbind( Constr.Aeq, t( Mu ) )
    ConstR.beq = cbind( Constr.beq, TargetExp[ k ] + t( Mu ) %*% w_b )

    w = MaxEntropy( G , w_b , w_0 , ConstR )

    m = t( Mu ) %*% ( w - w_b )
    
    s = sqrt( t( w - w_b ) %*% S %*% ( w - w_b ) )
    
    v_ = G %*% ( w - w_b )
    TE_contr = v_ * v_ / s

    R_2 = max( 10^(-10) , TE_contr / colSums( TE_contr ) )
    Ne = exp( -R_2 * log( R_2 ) )
    
    Weights = cbind( Weights, w )
    m_s = cbind( m_s, m )
    s_s = cbind( s_s, s )
    R_2_s = cbind( R_2_s, R_2 )
    Ne_s = cbind( Ne_s, Ne )
  }  
  return( list( Weights = Weights, Ne_s = Ne_s, R_2_s = R_2_s, m_s = m_s, s_s = s_s ) )
}