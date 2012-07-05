normalizeProb = function( p )
{
  tol = 1e-20
  tmp = p
  tmp[ tmp < tol ] = tol
  normalizedp = exp( log( tmp ) - log( sum( tmp ) ) )
    
  return( normalizedp )
}

subIntervals = function( x )
{
  n = nrow( x )
  x = t(x)
  xMesh = rep( NaN , n + 1)
  xMesh[ 1 ]     = x[ 1 ]
  xMesh[ n + 1 ] = x[ n ]
  xMesh[ 2:n ]   = x[2:n] - 0.5 * ( x[ 2:n ] - x[ 1:n-1 ] ) # simple product
    
  # cadlag mesh 
  xUB = xMesh[ -1 ] - 2.2e-308 # right
  xLB = xMesh[ -(n + 1) ] # left
    
  return( list( xLB = xLB , xUB = xUB ) )
}


integrateSubIntervals = function( x , cdf )
{
  bounds = subIntervals( x )
    
  p = (cdf( bounds$xUB ) - cdf( bounds$xLB )) / ( bounds$xUB - bounds$xLB ) # element by element division
    
  return( p )
}

hermitePolynomial = function( n )
{
  # convert last object to matrix
  # initialize p based on its expected dimension
  p = zeros(n+1,n+1)
  p[1, 1] = 1.0
  p[2, 1:2] = c(2,0)
  
  if( n >= 2 ) {
    for (k in 2:n)
    {
      p[ k + 1, 1:(k + 1)] = 2 *  cbind(t(p[k, 1:k]), 0)  - 2 * (k-1) * cbind(0, 0, t(p[k-1, 1:k-1]))
    }
  }
    
  return( p )  
}

gaussHermiteMesh = function( J )
{
  p = hermitePolynomial( J )
  x = polyroot( sort( p[ nrow(p), ] ) )
  x = Re(x) 
                
  return( x )
}

# Silverman bandwidth
kernelbw = function( xi )
{
  N      = length(xi)
  prop   = 1.0
  sig    = std( as.numeric( xi ) )
  iqrSig = 0.7413 * IQR(xi, type = 5)

  if (max(iqrSig) == 0) {
    iqrSig = sig
  }

  bw = prop * min(sig, iqrSig) * N^(-1 / (4 + 1));
  
  return ( bw )
}

kernelcdf = function( x, xi, bw, wi )
{
  n = length(xi)
  nargin <- length(as.list(match.call())) -1
  if ( nargin < 4 || isempty( wi ) ) { wi = ones(n, 1) / n }

  if ( nargin < 3 ) { bw = kernelbw(xi) }

  p = zeros( size( x ) )
  for ( i in 1:n ) {
    p = p + exp( log( wi[i] ) + log( pnorm( x, xi[i], bw ) ) )
  }
  
  return ( p )
}

kernelpdf = function( x, xi, bw, wi )
{
  n = length(xi)
  nargin <- length(as.list(match.call())) -1
  if ( nargin < 4 || isempty( wi ) ) { wi = ones(n, 1) / n }

  if ( nargin < 3 ) { bw = kernelbw(xi) }

  p = zeros( size( x ) )
  for ( i in 1:n ) {
    p = p + wi(i) * dnorm( x, xi(i), bw )
  }
}

kernelinv = function( p, xi, bw, wi )
{
  nargin <- length(as.list(match.call())) -1
  emptyMatrix = matrix( ,nrow = 0, ncol = 0)
  if ( nargin < 4 || isempty(wi) ) { wi = emptyMatrix }
  if ( nargin < 3 || isempty(bw) ) { bw = kernelbw(xi) }

  sortp = sort(p)

  if ( length(p) < 10 ) {
    # case with only few points by treating each point seperately
    x = zeros( dim( p ) )
    for ( i in 1:length(p) ) {
      x(i) = uniroot(function( x ) private_fun(x, xi, bw, wi, p[1]), 0)
    }
  }
  else {
  # case with many points by interpolation, find x_min and x_max
  x_min = uniroot(function( x ) private_fun(x, xi, bw, wi, sortp[1]), 0 )
  x_max = uniroot(function( x ) private_fun(x, xi, bw, wi, sortp[ nrow(sortp) ]), 0 )

  # mesh for x values
  x_ = seq( x_min - 0.1 * abs(x_min), x_max + 0.1 * abs(x_max), len = 500 )

  # evaluates the mesh on these values
  y_ = kernelcdf( x_, xi, bw, wi )

  # interpolation
  x = approx ( t( y_ ), t( x_ ), xout = t( p ) )
  }
}

private_fun = function( x, xi, bw, wi, p )
{
  f = kernelcdf( x, xi, bw, wi ) - p
  return( f )
}            