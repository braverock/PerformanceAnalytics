# This script estimates the prior of a hedge fund return and processes extreme views on CVaR 
# according to Entropy Pooling
# This script complements the article
#  "Fully Flexible Extreme Views"
#	by A. Meucci, D. Ardia, S. Keel
#	available at www.ssrn.com
# The most recent version of this code is available at
# MATLAB Central - File Exchange

# IMPORTANT - This script is about the methodology, not the input data, which has been modified

xi = as.matrix( 100 * data[, 2] )
n = nrow(xi)

# bandwidth
bw = kernelbw(xi)

% weights
lambda = log(2) / (n / 2)
wi = as.matrix( exp( -lambda * ( n - seq( from=n, to=1 ) ) )
wi = as.matrix( apply( wi, 2, rev ) / sum( wi ) )
                
# Prior market model
# kernel density

market.mu  = mean(xi)
market.pdf = function ( x ) kernelpdf( x, xi, bw, wi )
market.cdf = function ( x ) kernelcdf( x, xi, bw, wi )
market.inv = function ( x ) kernelinv( x, xi, bw, wi )
market.VaR95 = market.inv(0.05)
market.CVaR95 = integrate( function( x ) (x %*% market.pdf( x ) ), -100, market.VaR95 ) / 0.05;

# numerical (Gauss-Hermite grid) prior 
tmp = ( ghqx - min( ghqx ) )/( max( ghqx ) - min( ghqx ) ) # rescale GH zeros so they belong to [0,1]
epsilon = 1e-10
Lower = market.inv( epsilon )
Upper = market.inv( 1 - epsilon )
X  = Lower + tmp * ( Upper - Lower ) # rescale mesh
                
p = integrateSubIntervals( X , market.cdf )
p = normalizeProb( p )
J = length( X )
                
# Entropy posterior from extreme view on mean and CVaR
                
                
                