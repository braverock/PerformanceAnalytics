#' Transforms the first n raw moments into the first n central moments
#'
#' step 6 of projection process: 
#' 
#' compute multi-period central moments. 
#' 
#' Note the first central moment defined as expectation.
#'
#' \deqn{\tilde{ \mu } ^ {\big(n\big)} _{X}  \equiv E \big\{ X^{n} \big\},
#' \\ \mu ^{ \big(n\big) }_{X}  \equiv  \sum_0^{n-1}  \big(-1\big)^{n-k}   \mu ^{n-k}_{X}  \tilde{ \mu }^{k}_{X} +  \tilde{ \mu }_{X}^{n}   }
#' 
#' @param  mu_   the raw (multi-period) non-central moment of Y-t
#' @return mu    (multi-period) central moment of Y-t
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' @references
#' A. Meucci - "Exercises in Advanced Risk and Portfolio Management". See page 9
#' Symmys site containing original MATLAB source code \url{http://www.symmys.com}
#' @export
Raw2Central = function( mu_ )
{
  N = length( mu_ )
  mu = mu_
    
  for ( n in 2:N )
  {
    mu[n] = ((-1)^n) * (mu_[1])^(n) 
    for ( k in 1:(n-1) )
    {
      if ( n != 1 ) { mu[n] =  mu[n] + choose( n , k ) * ( ( -1 ) ^ ( n - k ) ) * mu_[k] * ( mu_[ 1 ] ) ^ ( n - k ) }
    }
    mu[n] = mu[n] + mu_[n]
  }
    
  return( mu = mu )
}

#' Transforms cumulants of Y-t into raw moments
#'
#' step 5 of the projection process: 
#' 
#' From the cumulants of Y we compute the raw non-central moments of Y
#' 
#' We do so recursively by the identity in formula (24) which follows from applying (21) and re-arranging terms
#'
#' \deqn{ \tilde{ \mu } ^{ \big(n\big) }_{Y} 
#' \equiv \kappa^{ \big(n\big) }_{Y}  +  \sum_{k=1}^{n-1} (n-1)C_{k-1}
#' \kappa_{Y}^{ \big(k\big) }   \tilde{ \mu } ^{n-k}_{Y}  }
#' 
#' @param  ka     cumulants of Y
#' @return mu_    the raw non-central moments of Y
#' 
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' @references
#' A. Meucci - "Annualization and General Projection of Skewness, Kurtosis and All Summary Statistics" - formula (24)
#' Symmys site containing original MATLAB source code \url{http://www.symmys.com/node/136}
#' @export
Cumul2Raw = function( ka )
{
  N = length( ka )
  mu_ = ka    
    
  for ( n in 1:N )
  {
    mu_[n] = ka[n]
    for ( k in 1:(n-1) ) 
    {
      if ( n != 1 ) { mu_[n] = mu_[n] + choose(n-1,k-1) * ka[k] * mu_[n-k] }
    }
  }    
  return( mu_ = mu_ )
}

#' Transforms raw moments into cumulants
#'
#' Step 3 of the projection process: From the non-central moments of X-t, we compute the cumulants. 
#' 
#' 
#' This process follows from the Taylor approximations for any small z and ln(1+x)~x for any small x,
#' and from the definition of the first cumulant in (17). The we apply recursively the identity
#' in formula (21). See Kendall and Stuart (1969)
#'
#' \deqn{ \kappa^{ \big(n\big) }_{X}   \equiv \tilde{ \mu } ^{ \big(n\big) }_{X} -  \sum_{k=1}^{n-1} (n-1)C_{k-1}  \kappa_{X}^{ \big(k\big) }   \tilde{ \mu } ^{n-k}_{X} }
#' 
#' @param mu_       non-central moments of the invariant X-t
#' @return ka       cumulants of X-t
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' @references
#' A. Meucci - "Annualization and General Projection of Skewness, Kurtosis and All Summary Statistics" - formula (21)
#' Symmys site containing original MATLAB source code \url{http://www.symmys.com/node/136}
#' @export
Raw2Cumul = function( mu_ )
{
  N = length( mu_ )
  ka = mu_
    
  for ( i in 1:N )
  {
    ka[i] = mu_[i]
    for ( k in 1:(i-1) ) 
    { 
      if ( i != 1 ) { ka[i] = ka[i] - choose(i-1,k-1) * ka[k] * mu_[i-k] }
    }
  }
    
  return( ka = ka )
}

#' Transforms first n central moments into first n raw moments (first central moment defined as expectation)
#'
#' step 2 of projection process: From the central moments of step 1, we compute the non-central moments. To do so we start
#' with the first non-central moment and apply recursively an identity (formula 20)
#'
#' \deqn{ \tilde{ \mu }^{ \big(1\big) }_{X} \equiv \mu ^{\big(1\big)}_{X}
#' \\ \tilde{ \mu }^{ \big(n\big) }_{X}  \equiv \mu ^{n}_{X} \sum_{k=0}^{n-1}  \big(-1\big)^{n-k+1}   \mu ^{n-k}_{X}  \tilde{ \mu }^{\big(k\big)}_{X} }

#' @param   mu      a vector of central moments
#' @return  mu_     a vector of non-central moments
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' @references 
#' A. Meucci - "Exercises in Advanced Risk and Portfolio Management". See page 10.
#' Symmys site containing original MATLAB source code \url{http://www.symmys.com}
#' @export
Central2Raw = function( mu )
{
  N = length( mu )
  mu_ = mu
    
  for ( i in 2:N ) # we use the notation 'i' instead of 'n' as in Meucci's code so we can browser
  {
    mu_[i] = ( ( -1 ) ^ ( i + 1 ) ) * ( mu[1] ) ^ (i)
        
    for ( k in 1:(i-1) )
    {
      mu_[i] =  mu_[i] + choose(i,k) * ((-1)^(i-k+1)) * mu_[k] * (mu_[1])^(i-k)
    }        
    mu_[i] = mu_[i] + mu[i]
  }
    
  return ( mu_ = mu_ )    
}

#' Compute summary stats
#'
#' step 0 in projection process: Compute summary stats (mean, skew, kurtosis, etc.) of the invariant X-t
#' step 1 in the project process We collect the first 'n' central moments of the invariant X-t. 
#'
#' @param    X    an invariant
#' @param    N    the number of order statistics to collect
#'
#' @return   ga   standardized statistics
#' @return   mu   central moments
#' @export
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' @export
SummStats = function( X , N )
{
  suppressWarnings( library( matlab ) ) # otherwise, stops with "Error: (converted from warning) package 'matlab' was built under R version 2.13.2"
  library( moments )        
    
  # step 1: compute central moments based on formula 15
  mu = zeros( 1 , N )
  mu[ 1 ] = mean( X )  
  for ( n in 2:N ) { mu[n] = moment( X , central = TRUE , order = n ) } # mu(2:n) contains the central moments. mu(1) is the mean
    
  # step 0: compute standardized statistics 
  ga = mu
  ga[ 2 ] = sqrt( mu[2] )
  for ( n in 3:N ) # we focus on case n >= 3 because from the definition of central moments (15) and from (3) that i) the first central moment is the mean of the invariant X-t, and ii) the second central moment is standard deviaiton of the of the invariant X-t
  {
    ga[n] = mu[n] / ( ga[2]^n ) # based on formula 19. ga[1] = mean of invariant X-t, ga[2] = sd, ga[3] = skew, ga[4] = kurtosis...
  }    
    
  return( list( ga = ga , mu = mu ) )
}

#' Calculates the population standard deviation
#'
#' Calculates the population standard deviation dividing by 'n' instead of 'n-1' equivalent to Matlab
#'
#' @param   x    a generic numeric vector
#' @return  std  a numeric with the population standard deviaiton of the generic numeric
#' @export
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
std = function( x ) { ( sum( ( x - mean( x ) ) ^ 2 ) / length( x ) ) ^.5 }

#' Generate arbitrary distribution of a shifted-lognormal invariant
#' 
#' \deqn{X = a +  e^{ m + sZ }} (formula 14)
#'
#' @param   J    a numeric with the number of scenarios
#' @param   a    a numeric with the location shift parameter. Mean of distribution will be exp(a)
#' @param   m    log of the mean of the distribution
#' @param   s    log of the standard deviation of the distribution
#'
#' @return  X    a numeric vector with i.i.d. lognormal samples based on parameters J, a, m, and s where X = a + exp( m + s * Z )
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' @export
GenerateLogNormalDistribution = function( J, a, m, s )
{
  Z = rnorm( J / 2 , 0 , 1 ) # create J/2 draws from the standard normal 
  Z = c( Z , -Z) / std( Z ) # a Jx1 numeric vector
  X = a + exp( m + s * Z ) # a Jx1 numeric vector
  
  return( X = X )
}

