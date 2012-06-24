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
  p<-matrix(nrow=2,ncol=2)    
  p[1, 1] = 1.0
  p[2, 1:2] = c(2,0)
    
  for (k in 2:n)
  {
    p[ k + 1, 1:k + 1] = 2 * cbind(p[k, 1:k], 0) - 2 * (k-1) * cbind(0, 0, p(k-1, 1:k-1))
  }
    
  return( p )  
}
