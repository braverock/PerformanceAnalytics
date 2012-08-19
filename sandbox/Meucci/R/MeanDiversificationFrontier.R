#' This function generates the first eigen vector
#' 
#' @param S     Covariance Matrix
#' @param A     Conditioning Matrix
#'
#' @return e    First Eigen Vector
#'
#' @references 
#' A. Meucci - "Managing Diversification", Risk Magazine, June 2009
#' \url{http://ssrn.com/abstract=1358533}
#' @author Manan Shah \email{mkshah@@cmu.edu}
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

#' This function computes the conditional principal portfolios
#' 
#' @param S     Covariance Matrix
#' @param A     Conditioning Matrix
#'
#' @return      a list containing
#' @return E    a matrix containing conditional principal portfolios composition
#' @return L    a matrix containing conditional principal portfolios variances
#' @return G    map weights -> conditional diversification distribution (square root of, not normalized)
#'
#' \deqn{ e_{n}  \equiv   argmax_{ e'e  \equiv 1 }  \big\{ e' \Sigma e \big\} s.t.  e' \Sigma  e_{j}  \equiv 0 }
#' @references 
#' A. Meucci - "Managing Diversification", Risk Magazine, June 2009 - Formula (12)
#' \url{http://ssrn.com/abstract=1358533}
#' @author Manan Shah \email{mkshah@@cmu.edu}
GenPCBasis = function( S , A )
{  
  if ( length( A ) == 0 )
  {
    N = nrow( S )
    L = rep( 0, N )
    K = 0
    tmp = eigen( S )
    E_ = tmp$vectors
    L_ = diag( tmp$values )
    E = E_
    for ( n in 1:N )
    {
      E[ , n ] = E_[ , N - n + 1 ]
      L[ n ] = L_[ N - n + 1 , N - n + 1 ]
    }
  }
  else
  {
    K = nrow( A )
    N = ncol( A )
    emptyMatrix = matrix( ,nrow = 0, ncol = 0 )
    E = emptyMatrix
    B = A
    for ( n in 1:N - K )
    {
      if ( length( E ) != 0 )
      {
        B = rbind( A, t( E ) %*% S )
      }
      e = GenFirstEigVect( S , B )
      E = cbind( E , e )
    }
    
    for ( n in N - K + 1:N )
    {
      B = t( E ) %*% S
      e = GenFirstEigVect( S , B )
      E = cbind( E , e )
    }

    # swap order
    E = cbind( E[ , N - K + 1:N ], E[ , 1:N - K ] )
  }  
 
  v = t( E ) %*% as.matrix( S ) %*% E
  L = diag( v )

  G = diag( sqrt( L ), nrow = length( L ) ) %*% solve( E )
  G = G[ K + 1:N , ]
  return( list( E = E, L = L, G = G ) )
}

#' This function computes the extreme frontier
#' 
#' @param G       map weights -> conditional diversification distribution (square root of, not normalized)
#' @param w_b     a matrix containing the benchmark weights
#' @param w_0     a matrix containing the initial portfolio weights
#' @param Constr  a list containing the equality and inequality constraints
#'
#' @return x      a numeric containing the maximum entropy
#'
#' \deqn{  N_{ent}  \equiv exp \big(-\sum_{n=k+1}^N  p_{n} ln p_{n}  \big),
#'  w_{  \varphi }  \equiv  argmax_{w \in C,   \mu'w  \geq  \varphi  }  N_{ent}   \big(w\big) }
#' @references 
#' A. Meucci - "Managing Diversification", Risk Magazine, June 2009 - Formula (18,19)
#' \url{http://ssrn.com/abstract=1358533}
#' @author Manan Shah \email{mkshah@@cmu.edu}
MaxEntropy = function( G , w_b , w_0 , Constr )
{
  library( nloptr )
  # Nested function that computes fitness
  nestedfun = function( x )
  {
    v_ = G %*% ( x - as.matrix( w_b ) )
    p = v_ * v_
    R_2 = pmax( 10^(-10), p / colSums( p ) )
    Minus_Ent = t( R_2 ) * log( R_2 )
    
    # evaluate gradient
    gradient = rbind( Constr$b - Constr$A %*% x , Constr$beq - Constr$Aeq %*% x )
    
    return( list( objective = Minus_Ent , gradient = gradient ) )
  }
  
  local_opts <- list( algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1.0e-6 , 
                      check_derivatives = FALSE , #check_derivatives_print = "all" , 
                      eval_f = nestedfun )
  x = nloptr( x0 = w_0 , eval_f = nestedfun ,
              opts = list( algorithm = "NLOPT_LD_AUGLAG" , local_opts = local_opts ,
                print_level = 2 , maxeval = 1000 , 
                check_derivatives = FALSE , #check_derivatives_print = "all" , 
                           xtol_rel = 1.0e-6 ) )
  return( x )
}

#' This function computes the mean diversification efficient frontier
#' 
#' @param S       Covariance Matrix
#' @param Mu      a matrix containing the expectations 
#' @param w_b     a matrix containing the benchmark weights
#' @param w_0     a matrix containing the initial portfolio weights
#' @param Constr  a list containing the equality and inequality constraints
#'
#' @return        a list containing
#' @return Weights
#' @return Ne_s
#' @return R_2_s
#' @return m_s
#' @return s_S
#'
#' @references 
#' A. Meucci - "Managing Diversification", Risk Magazine, June 2009
#' \url{http://ssrn.com/abstract=1358533}
#' @author Manan Shah \email{mkshah@@cmu.edu}
MeanTCEntropyFrontier = function( S , Mu , w_b , w_0 , Constr )
{
  emptyMatrix = matrix( ,nrow = 0, ncol = 0)
  # compute conditional principal portfolios
  GenPCBasisResult = GenPCBasis( S, emptyMatrix )

  # compute frontier extrema
  library( limSolve )
  w_MaxExp = as.matrix( linp( E = Constr$Aeq , F = Constr$beq , G = -1*Constr$A , H = -1*Constr$b, Cost = -t( Mu ) , ispos = FALSE)$X )
  MaxExp = t( Mu ) %*% ( w_MaxExp - as.matrix( w_b ) )

  w_MaxNe = MaxEntropy( GenPCBasisResult$G , w_b , w_0 , Constr )
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
    ConstR$Aeq = cbind( Constr$Aeq, t( Mu ) )
    ConstR$beq = cbind( Constr$beq, TargetExp[ k ] + t( Mu ) %*% w_b )

    w = MaxEntropy( GenPCBasisResult$G , w_b , w_0 , ConstR )

    m = t( Mu ) %*% ( w - w_b )
    
    s = sqrt( t( w - w_b ) %*% S %*% ( w - w_b ) )
    
    v_ = GenPCBasisResult$G %*% ( w - w_b )
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