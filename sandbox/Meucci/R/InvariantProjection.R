

#' Transforms raw moments into central moments
#'
#' step 6 of projection process: compute multi-period central moments. Note the first central moment defined as expectation.
#'
#' @param  mu_   the raw (multi-period) non-central moment of Y-t
#' @return mu    (multi-period) central moment of Y-t
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' TODO FIXME check these against the central moment functions in PerformanceAnalytics
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
#' step 5 of the projection process: From the cumulants of Y we compute the raw non-central moments of Y
#' We do so recursively by the identity in formula (24) which follows from applying (21) and re-arranging terms
#'
#' @param  ka     cumulants of Y
#' @return mu_    the raw non-central moments of Y
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
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
#' This process follows from the Taylor approximations for any small z and ln(1+x)~x for any small x,
#' and from the definition of the first cumulant in (17). The we apply recursively the identity
#' in formula (21). See Kendall and Stuart (1969)
#'
#' @param mu_       non-central moments of the invariant X-t
#' @return ka       cumulants of X-t
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
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

#' Transforms central moments into raw moments (first central moment defined as expectation)
#'
#' step 2 of projection process: From the central moments of step 1, we compute the non-central moments. To do so we start
#' with the first non-central moment and apply recursively an identity (formula 20)
#'
#' @param   mu      a vector of central moments
#' @return  mu_     a vector of non-central moments
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
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
#' Calculates the population standard deviaiton dividing by 'n' instead of 'n-1' equivalent to Matlab
#'
#' @param   x    a generic numeric vector
#' @return  std  a numeric with the population standard deviaiton of the generic numeric
#' @export
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
std = function( x ) { ( sum( ( x - mean( x ) ) ^ 2 ) / length( x ) ) ^.5 }

#' Annualization and Projection algorithm for invariant
#'
#' Project summary statistics to arbitrary horizons under i.i.d. assumption
#' SYMMYS - Last version of article and code available at http://symmys.com/node/136
#' Project summary statistics to arbitrary horizons under i.i.d. assumption
#' see Meucci, A. (2010) "Annualization and General Projection of Skewness, Kurtosis and All Summary Statistics"
#' GARP Risk Professional, August, pp. 52-54
#'
#' @param    N    a numeric with the number of the first N stadardized summary statistics to project
#' @param    K    a numeric with an arbitrary projection horizon
#' @param    X    a numeric vector consisting of a generic (additive) invariant the 
#'                  follows the general linear and square-root rules for projecting means and volatility
#'
#' @return   Ga   a numeric vector with the first 'N' order statistics projected to the horizon 'K'
#' @export
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' @examples
#'           X = GenerateLogNormalDistribution( J = 100000 , a = 01 , m = .2 , s = .4 ) # X = a + exp( m + s * Z ) # generate log-normal distribution
#'           moments = ProjectInvariant( N = 6 , K = 251 , X )
ProjectInvariant = function( N = 6 , K = 251 , X )
{
    
    # TODO: Expectations on outputs
    # Ga[1] should equal K*mean(X)
    # Ga[2] should equal sqrt(K)*std(X)
    
    # show distribution of the invariant. Invariance test: The three distributions should be very similar
    hist( X , 50 , freq = FALSE , main = "Distribution of Invariant" , xlab = "X" )                          # chart 1: distribution of invariant
    hist( X[ 1 : length( X ) / 2 ] , 50 , freq = FALSE , main = "Distribution (1st Half of Pop.)" , xlab = "X" )   # chart 2: distribution of invariant (1st-half of population)
    hist( X[ ( length( X ) / 2 ) : length( X ) ] , 50 , freq = FALSE , main = "Distribution  (2nd Half of Pop.)" , xlab = "X" ) # chart 3: distribution of invariant (2nd-half of population)
    
    # To compute the standardized summary statistics of Y we need to introduce
    # three sets of players, defined as follows for a generic random variable X: the
    # central moments (15), the non-central moments (16), and the cumulants (17) for each order n
    
    # step 0: compute single-period standardized statistics (mean, volatility, skew, kurtosis, etc.) step 1: compute central moments
    stats = SummStats( X , N ) # returns ga (standardized statistics), and mu (the central moments)
    
    # step 2: From the central moments of step 1, we compute the non-central moments. To do so we start
    # with the first non-central moment and apply recursively an identity (formula 20)
    
    # step 3 of the projection process: From the non-central moments of X-t, we compute the cumulants of X-t.
    # This process follows from the Taylor approximations for any small z and ln(1+x)~x for any small x,
    # and from the definition of the first cumulant in (17). The we apply recursively the identity
    # in formula (21). See Kendall and Stuart (1969)
    mu_ = Central2Raw( stats$mu )
    
    # step 4: Transform cumulants of X-t into the cumulants of the annualization/projetion Y = X1 + X2 + X3...
    ka = Raw2Cumul( mu_ ) # compute single-period cumulants
    
    # now compute multi-period cumulants
    # Since X-t is an invariant, all the X-t's are i.i.d. therefore the projected cumulants = k * Ka
    # See also Duc and Schordereret (2008)
    Ka = K * ka
    
    # step 5: compute multi-period non-central moments
    Mu_ = Cumul2Raw( Ka ) # Transforms cumulants of Y-t into raw moments of Y-t
    
    # step 6: compute multi-period central moments
    Mu = Raw2Central( Mu_ )
    
    # step 7: compute multi-period projected standardized statistics of Y-t
    Ga = Mu
    
    Ga[2] = sqrt( Mu[2] )
    
    for ( n in 3:N )
    {
        Ga[ n ] = Mu[ n ] / ( Ga[ 2 ] ^ n )
    }
    
    print( Ga ) # TODO: add colnames - mean, sd, skew, kurtosis, ...
}

#' Generate arbitrary distribution of a shifted-lognormal invariant: X-t + a ~ LogN(m,s^2) (formula 14)
#'
#' @param   J    a numeric with the number of scenarios
#' @param   a    a numeric with the location shift parameter. Mean of distribution will be exp(a)
#' @param   m    log of the mean of the distribution
#' @param   s    log of the standard deviation of the distribution
#'
#' @return  X    a numeric vector with i.i.d. lognormal samples based on parameters J, a, m, and s where X = a + exp( m + s * Z )
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
GenerateLogNormalDistribution = function( J = 100000 , a = -1 , m = .2 , s = .4 )
{
    Z = rnorm( J / 2 , 0 , 1 ) # create J/2 draws from the standard normal
    
    Z = c( Z , -Z) / std( Z ) # a Jx1 numeric vector
    
    X = a + exp( m + s * Z ) # a Jx1 numeric vector
}
