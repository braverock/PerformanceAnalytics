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
{
    # convert last object to matrix
    # initialize p based on its expected dimension
    p<-matrix(nrow=2,ncol=2)    
    p[1, 1] = 1.0
    p[2, 1:2] = c(2,0)
    
    for (k in 2:n)
    {
        # convert last object to matrix
        # TODO FIXME BGP: this line is not R syntax
        #p[ k + 1, 1:k + 1] = 2 * (p[k, 1:k], 0] - 2 * (k-1) * [0, 0, p(k-1, 1:k-1)]
    }
    
    return( p )
    
}
