# This script verifies the correctness of the eigenvalue-eigenvector representation in terms of real matrices 
# for the transition matrix of an OU process discussed in A. Meucci (2009) 
# "Review of Statistical Arbitrage, Cointegration, and Multivariate Ornstein-Uhlenbeck"
# available at ssrn.com

# Code by A. Meucci, April 2009
# Most recent version available at www.symmys.com > Teaching > MATLAB

N = 5
Theta = matrix( runif( N^2 ), 5, byrow = T )

tmp = eigen( Theta )
B = tmp$vectors
L = tmp$values

A = Re( B ) - Im( B )

Index_j = as.matrix( which( Im ( L ) != 0 ) )
L = diag( L )
  
G = L
for ( s in c( seq( from = 1, to = length( Index_j ), by = 2 ) ) )
{
  G[ Index_j[ s : (s+1) ] , Index_j[ s : (s+1) ] ] = rbind( c( 1 , 0 ), c( 0 , 1 ) ) * Re( L[ Index_j[s],Index_j[s] ] ) +
  rbind( c( 0 , 1 ), c( -1 , 0 ) ) * Im( L [ Index_j[s] , Index_j[s] ] ) 
}
Theta_ = A %*% G %*% solve( A )
