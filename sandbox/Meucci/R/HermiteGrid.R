#' Generates the normalized probability for an input probability value
#' 
#' @param p             a numeric value containing the probability value required to be normalized 
#'
#' @return normalizedp  a numeric value containing the normalized probability value
#'
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
normalizeProb = function( p )
{
  tol = 1e-20
  tmp = p
  tmp[ tmp < tol ] = tol
  normalizedp = exp( log( tmp ) - log( sum( tmp ) ) )
    
  return( normalizedp )
}

#' Generate the intervals containing jth point of the grid.
#' 
#' @param x     a vector containing the scenarios 
#'
#' @return      a list containing
#' @return xLB  a vector containing the lower bound of each interval
#' @return xUB  a vector containing the upper bound of each interval
#"
#' \deqn{ I_{j} \equiv  \big[ x_{j} -  \frac{x_{j} - x_{j-1} }{2}, x_{j} +  \frac{x_{j+1} - x_{j}}{2} \big)  }
#' @references 
#' A. Meucci - "Fully Flexible Extreme Views" - See formula (17)
#' \url{http://ssrn.com/abstract=1542083}
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
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

#' Integrate the subinterval for the given cumulative distribution function to get the equivalent probability
#' 
#' @param x     a vector containing the data points
#' @param cdf   the cumulative distribution function
#'
#' @return p    a vector containing the cdf evaluated for each of the subintervals
#'
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
integrateSubIntervals = function( x , cdf )
{
  bounds = subIntervals( x )
    
  p = (cdf( bounds$xUB ) - cdf( bounds$xLB )) / ( bounds$xUB - bounds$xLB ) # element by element division
    
  return( p )
}

#' Generate a Hermite Polynomial of order n
#' 
#' @param n   A numeric defining the order of the Hermite Polynomial
#'
#' @return p  A vector containing the Hermite Polynomial of order n
#' \deqn{ H_{j} (x)  \equiv  (-1)^{J}   e^{ \frac{ -x^{2} }{2} }  \frac{ d^{J} e^{ \frac{ -x^{2} }{2} }}{ dx^{J} } }
#' @references 
#' A. Meucci - "Fully Flexible Extreme Views" - See formula (20)
#' \url{http://ssrn.com/abstract=1542083}
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
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

#' Generates grid reprensentation of a distribution according to the method suggested by Meucci and inspired from
#' Gauss-Hermite quadratures.
#'
#' Grid representation by this method is an alternative to representing distribution of the market, such as 
#' importance sampling or stratified sampling.However, these techniques focus on sub-domains of the distribution, and thus,
#' in order to apply such methods, we must forego the full flexibility on the specification of the views. A different approach, 
#' which preserves the generality of the views specification, consists in selecting the scenarios x_j deterministically as a 
#' grid and then associate with each of them the suitable probability p_j ( integrated over I_j ). The the generic interval 
#' _j contains the j-th point of the grid. Once the grid is defined, the entropy optimization can be applied to 
#' replace p_j with the new posterior probabilities to reflect the views. We generate the grid here using the 
#' Gauss-Hermite quadrature method.
#'
#' @param J   a numeric containing the number of points on the grid
#'
#' @return x  a matrix containing the zeroes of Hermite polynomials as a function of polynomial degree
#'
#' @references 
#' A. Meucci - "Fully Flexible Extreme Views".
#' \url{http://ssrn.com/abstract=1542083}
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
gaussHermiteMesh = function( J )
{
  p = hermitePolynomial( J )
  x = polyroot( sort( p[ nrow(p), ] ) )
  x = Re(x) 
                
  return( x )
}

#' Generates bandwidth of a Gaussian Kernel Density Estimator based on Silverman's rule of thumb
#'
#' @param xi  a vector containing the data set for which the bandwidth has to be calculated
#'
#' @return bw a numeric signifying the bandwidth
#'
#' @author Manan Shah \email{mkshah@@tepper.cmu.edu}
kernelbw = function( xi )
{
  N      = length(xi)
  prop   = 1.0
  sig    = sd( as.numeric( xi ) )
  iqrSig = 0.7413 * IQR(xi, type = 5)

  if (max(iqrSig) == 0) {
    iqrSig = sig
  }

  bw = prop * min(sig, iqrSig) * N^(-1 / (4 + 1));
  
  return ( bw )
}

#' Evaluates cumulative distribution function for the input numeric value
#'
#' @param x   a vector containing the numeric values for which cumulative distribution function has to be evaluated
#' @param xi  a vector containing the data set
#' @param bw  a numeric value containing Silverman bandwidth of the given data set
#' @param wi  a vector containing weights
#'
#' @return p  a numeric containing the cumulative probability distribution value of length equal to x
#'
#' @author Manan Shah \email{mkshah@@tepper.cmu.edu}
kernelcdf = function( x, xi, bw, wi )
{
  n = length(xi)
  nargin <- length(as.list(match.call())) -1
  if ( nargin < 4 || length( wi ) == 0 ) { wi = ones(n, 1) / n }

  if ( nargin < 3 ) { bw = kernelbw(xi) }

  p = rep( 0, length( x ) )
  for ( i in 1:n ) {
    p = p + exp( log( wi[i] ) + log( pnorm( x, xi[i], bw ) ) )
  }
  
  return ( p )
}

#' Evaluates probability distribution function for the input numeric value
#'
#' @param x   a vector containing the numeric values for which probability distribution function has to be evaluated
#' @param xi  a vector containing the data set
#' @param bw  a numeric value containing Silverman bandwidth of the given data set
#' @param wi  a vector containing weights
#'
#' @return p  a vector containing the probability distribution function value of length equal to x
#'
#' @author Manan Shah \email{mkshah@@tepper.cmu.edu}
kernelpdf = function( x, xi, bw, wi )
{
  n = length(xi)
  nargin <- length(as.list(match.call())) -1
  if ( nargin < 4 || length( wi ) == 0 ) { wi = ones(n, 1) / n }

  if ( nargin < 3 ) { bw = kernelbw(xi) }

  p = rep( 0, length( x ) )
  for ( i in 1:n ) {
    p = p + wi[i] * dnorm( x, xi[i], bw )
  }
  
  return( p )
}

#' Evaluates inverse probability distribution function for the input probability in order to get the data point
#'
#' @param p   a vector containing the probabilities corresponding to which the data points have to be evaluated
#' @param xi  a vector containing the data set
#' @param bw  a numeric value containing Silverman bandwidth of the given data set
#' @param wi  a vector containing weights
#'
#' @return x  a vector containing the evaluated numeric values of length equal to p
#'
#' @author Manan Shah \email{mkshah@@tepper.cmu.edu}
kernelinv = function( p, xi, bw, wi )
{
  nargin <- length(as.list(match.call())) -1
  emptyMatrix = matrix( ,nrow = 0, ncol = 0)
  if ( nargin < 4 || length( wi ) == 0 ) { wi = emptyMatrix }
  if ( nargin < 3 || length( bw ) == 0 ) { bw = kernelbw(xi) }

  sortp = sort(p)

  if ( length(p) < 10 ) {
    # case with only few points by treating each point seperately
    x = rep( 0, length( p ) )
    for ( i in 1:length(p) ) {
      x[i] = uniroot(function( x ) private_fun(x, xi, bw, wi, p[1]), c( -100, 100 ) )$root
    }
  }
  else {
  # case with many points by interpolation, find x_min and x_max
  x_min = uniroot(function( x ) private_fun(x, xi, bw, wi, sortp[1]), c( -100, 100 ) )$root
  x_max = uniroot(function( x ) private_fun(x, xi, bw, wi, sortp[ nrow(sortp) ]), c( -100, 100 ) )$root

  # mesh for x values
  x_ = seq( x_min - 0.1 * abs(x_min), x_max + 0.1 * abs(x_max), len = 500 )

  # evaluates the mesh on these values
  y_ = kernelcdf( x_, xi, bw, wi )

  # interpolation
  x = approx ( t( y_ ), t( x_ ), xout = t( p ) )
  }
  
  return( x )
}

#' Evaluates the difference between calculated cumulative distribution function for a data point and the 
#' true value
#'
#' @param x   a vector containing the data points for which cdf has to be evaluated
#' @param xi  a vector containing the data set
#' @param bw  a numeric value containing Silverman bandwidth of the given data set
#' @param wi  a vector containing weights
#' @param p   a vector containing the true probabilities
#'
#' @return f  a vector containing the difference between x and p which is of length equal to x or p
#'
#' @author Manan Shah \email{mkshah@@tepper.cmu.edu}
private_fun = function( x, xi, bw, wi, p )
{
  f = kernelcdf( x, xi, bw, wi ) - p
  return( f )
}            