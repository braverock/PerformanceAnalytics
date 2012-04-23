pHist = function( X , p , nBins )    
    {      
    if ( length( match.call() ) < 3 )
        {
        J = size( X , 1 )        
        nBins = round( 10 * log(J) )
        }
    
    dist = hist( x = X , breaks = nBins , freq = FALSE , main = "Portfolio return distribution" )
    n = dist$counts
    x = dist$breaks    
    D = x[2] - x[1]
    
    N = length(x)
    np = zeros(N , 1)
    
    for (s in 1:N)
        {
        # The boolean Index is true is X is within the interval centered at x(s) and within a half-break distance
        Index = ( X >= x[s] - D/2 ) & ( X <= x[s] + D/2 )    
        # np = new probabilities?
        np[ s ] = sum( p[ Index ] )
        f = np/D
        }
    
    barplot( f , x , 1 )
          
    h = emptyMatrix
    
    return( list( h = h , f = f , x = x ) )
    }

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
    n = length( x )
    xMesh = rep( NaN , n + 1)
    xMesh[ 1 ]     = x[ 1 ]
    xMesh[ n + 1 ] = x[ n ]
    xMesh[ 2:n ]   = x[2:n] - 0.5 * ( x[ 2:n ] - x[ 1:n-1 ] ) # matrix product or simple product?
    
    # cadlag mesh 
    xUB = xMesh[ -1 ] - 2.2e-308 # right
    xLB = xMesh[ -n ] # left
    
    return( list( xLB = xLB , xUB = xUB ) )
    }

 
integrateSubIntervals = function( x , cdf )
    {
    bounds = subIntervals( x )
    
    cdfUB = cdf( bounds$xUB )
    cdfLB = cdf( bounds$xLB )
    
    p = (cdfUB - cdfLB) / ( bounds$xUB - bounds$xLB ) # element by element division
    
    return( p )
    }

Prior2Posterior = function( M , Q , M_Q , S , G , S_G )
    {
    # Compute posterior moments
    
    if ( Q != 0 ) { M_ = M + S*t(Q) %*% solve( Q %*% S %*% t(Q) ) %*% ( M_Q - Q %*% M) }
    else { M_ = M }
    
    if ( G != 0 ) { S_ = S + (S %*% t(G)) %*% ( solve(G %*% S %*% t(G)) %*% S_G %*% solve(G %*% S %*% t(G)) - solve( G %*% S %*% t(G)) ) %*% (G %*% S) }
    else { S_ = S }
    
    return( list( M_ = M_ , S_ = S_ ) )
    }


hermitePolynomial = function( n )

# convert last object to matrix
# initialize p based on its expected dimension
p[1, 1] = 1.0
p[2, 1:2] = [2, 0]

for (k in 2:n)
    {
    # convert last object to matrix
    p[ k + 1, 1:k + 1] = 2 * [p[k, 1:k], 0] - 2 * (k-1) * [0, 0, p(k-1, 1:k-1)]
    }

return( p )

# This script compares the performance of plain Monte Carlo 
# versus grid in applying Entropy Pooling to process extreme views
# This script complements the article
#    "Fully Flexible Extreme Views" A. Meucci, D. Ardia, S. Keel available at www.ssrn.com
# The most recent version of this code is available at MATLAB Central - File Exchange
library(matlab)


####################################################################################
# Prior market model
####################################################################################
# analytical (normal) prior
emptyMatrix = matrix( nrow=0 , ncol=0 )
market.mu   = 0.0
market.sig2 = 1.0
market.pdf = function(x) dnorm( x , mean = market.mu , sd = sqrt(market.sig2) )
market.cdf = function(x) pnorm( x , mean = market.mu , sd = sqrt(market.sig2) )
market.rnd = function(x) rnorm( x , mean = market.mu , sd = sqrt(market.sig2) ) 
market.inv = function(x) qnorm( x , mean = market.mu , sd = sqrt(market.sig2) )

# numerical (Monte Carlo) prior 
monteCarlo = emptyMatrix
monteCarlo.J = 100000
monteCarlo.X = market.rnd( monteCarlo.J )
monteCarlo.p = normalizeProb( 1/monteCarlo.J * ones( monteCarlo.J , 1 ) )

# numerical (Gauss-Hermite grid) prior 
ghqMesh = emptyMatrix
load( file.choose() )
ghqx = ghqx[[1]]

tmp = ( ghqx - min( ghqx ) ) / ( max( ghqx ) - min( ghqx ) ) # rescale GH zeros so they belong to [0,1]
epsilon = 1e-10
Lower = market.inv( epsilon )
Upper = market.inv( 1-epsilon )
ghqMesh.X  = Lower + tmp*(Upper-Lower) # rescale mesh

p = integrateSubIntervals(ghqMesh.X , market.cdf)
ghqMesh.p = normalizeProb(p)
ghqMesh.J = length(ghqMesh.X) 

####################################################################################
# Entropy posterior from extreme view on expectation
####################################################################################
# view of the analyst
view = emptyMatrix
view.mu = -3.0 

# analytical (known since normal model has analytical solution)
truePosterior = emptyMatrix 
truePosterior = Prior2Posterior( market.mu, 1, view.mu, market.sig2, 0 )
truePosterior$pdf = function(x) dnorm( x, truePosterior.mu , sqrt(truePosterior.sig2) )

# numerical (Monte Carlo)
Aeq = rbind( ones( 1 , monteCarlo.J ) , t(monteCarlo.X) )
beq = rbind( 1 , view.mu )
monteCarloOptimResult = EntropyProg( monteCarlo.p , emptyMatrix , emptyMatrix , Aeq , beq )

monteCarlo.p_ = monteCarloOptimResult$p_
monteCarlo.KLdiv = monteCarloOptimResult$optimizationPerformance$ml

# numerical (Gaussian-Hermite grid)
Aeq = rbind( ones( 1 , ghqMesh.J ) , t( ghqMesh.X ) )
beq = rbind( 1 , view.mu )
ghqMeshOptimResult = EntropyProg( ghqMesh.J , emptyMatrix , emptyMatrix , Aeq , beq )

ghqMesh.p_ = ghqMeshOptimResult$p_
ghqMesh.KLdiv = ghqMeshOptimResult$optimizationPerformance$ml


####################################################################################
# Plots
####################################################################################
xmin = min(ghqMesh.X)
xmax = max(ghqMesh.X)
ymax = 1.0
xmesh = t(linspace(xmin, xmax, ghqMesh.J))

# Monte Carlo
plotDataMC = pHist( monteCarlo.X , monteCarlo.p_ , 50 )
plot( plotDataMC$x , plotDataMC$f , type = "l" )

# Gauss Hermite Grid
plotDataGHQ = pHist(ghqMesh.X, ghqMesh.p_ , 50 )
plot( plotDataGHQ$x , plotDataGHQ$f , type = "l" )

