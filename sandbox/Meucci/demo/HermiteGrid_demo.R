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
load( "ghq1000.rda" )

tmp = ( ghqx - min( ghqx ) ) / ( max( ghqx ) - min( ghqx ) ) # rescale GH zeros so they belong to [0,1]
epsilon = 1e-10
Lower = market.inv( epsilon )
Upper = market.inv( 1-epsilon )
ghqMesh.X  = Lower + tmp*(Upper-Lower) # rescale mesh

p = integrateSubIntervals(ghqMesh.X , market.cdf)
ghqMesh.p = normalizeProb(p)
ghqMesh.J = nrow(ghqMesh.X) 

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
ghqMeshOptimResult = EntropyProg( ghqMesh.p , emptyMatrix , emptyMatrix , Aeq , beq )

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
plotDataGHQ = pHist(data.matrix(ghqMesh.X), ghqMesh.p_ , 50 )
plot( plotDataGHQ$x , plotDataGHQ$f , type = "l" )

