# Annualization and Projection algorithm for invariant
#
# SYMMYS - Last version of article and code available at http://symmys.com/node/136
# Project summary statistics to arbitrary horizons under i.i.d. assumption
# see Meucci, A. (2010) "Annualization and General Projection of Skewness, Kurtosis and All Summary Statistics"
# GARP Risk Professional, August, pp. 52-54

N = 6   # a numeric with the number of the first N stadardized summary statistics to project
K = 100 # a numeric with an arbitrary projection horizon

J = 100000  # a numeric with the number of scenarios
a = -1      # a numeric with the location shift parameter. Mean of distribution will be exp(a)
m = 0.2     # log of the mean of the distribution
s = 0.4     # log of the standard deviation of the distribution
 
X = GenerateLogNormalDistribution(J, a, m, s)
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