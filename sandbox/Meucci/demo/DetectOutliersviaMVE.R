# Example 1: Two-asset example with -.9 correlation
# generate covariance matrix to sample "good returns"
sig = matrix( c( 1 , 1 ) , nrow = 1 ); r = -.9
correlationMatrix = rbind( cbind( 1 , r ) , cbind( r , 1 ) ) # generate 2x2 correlation matrix
covarianceMatrix = diag(sig) * correlationMatrix * diag(sig) ; rm( correlationMatrix ) ; rm( sig ) ; rm( r ) # convert to covariance matrix

# co-mingle sample from good covariance matrix with bad outliers and shuffle rows
corruptSample = NoisyObservations( 50 , 7 , covarianceMatrix , shuffle = TRUE ) # returns 50 good samples and 7 bad samples

# identify/detect number of outliers
result = DetectOutliersViaMVE( corruptSample )

# remove outliers
cleanSample = corruptSample[ c(-16, -57, -21, -39, -14, -52, -26 , -37 ),  ]              
DetectOutliersViaMVE( cleanSample )
result

# Example 2: Multi-asset example
# generate covariance matrix to sample "good returns"
library( Matrix )
numberOfStocks = 100
rm( result ) ; rm( covarianceMatrix ) ; rm( corruptSample )
covarianceMatrix = matrix( rnorm( numberOfStocks^2 , 0 , .1 ) , nrow = numberOfStocks , ncol = numberOfStocks )
covarianceMatrix = ( covarianceMatrix + t( covarianceMatrix ) ) / 2
covarianceMatrix = nearPD( covarianceMatrix , corr = FALSE )$mat
covarianceMatrix = as.matrix(covarianceMatrix)

# co-mingle sample from good covariance matrix with bad outliers and shuffle rows
corruptSample = NoisyObservations( 150 , 7 , covarianceMatrix , shuffle = FALSE ) 

# identify/detect number of outliers
result = DetectOutliersViaMVE( corruptSample )        
result

# print the index of the observation that is largest outlier
RejectOutlier( corruptSample )$rejected    