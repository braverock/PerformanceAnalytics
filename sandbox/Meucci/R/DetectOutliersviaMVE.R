#' Finds the "worst" outlier in a multivariate time series
#'
#' Finds the "worst" outlier in a multivariate time-series
#' We aim at finding the the observation x_t such that if we remove it from the
#' set {x_1 ... x_t } the determinant of the resulting sample covariance is reduced the most
#' This means that by dropping that observation the location-dispersion ellipsoid defined
#' by the sample mean and covariance shrinks the most
#' See Sec. 4.6.1 of "Risk and Asset Allocation" - Springer (2005), by A. Meucci
#' for the theory and the routine implemented below
#'
#' @param   sample      a vector containing the input dataset with outliers
#'
#' @return  rejected    a numeric indicating which observation in the index to reject
#'
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#'
#' @references 
#' \url{http://www.symmys.com}
#' See Meucci script for "RejectOutlier.m"
#' @export
RejectOutlier = function( sample )
{   
  library( matlab )
    
  # parameter checks
  if ( ncol( sample ) > nrow( sample ) ) { stop("The number of assets must be greater than number of observations (otherwise system is singular)") }    
    
  # initialize parameters
  T = nrow( sample )
  m = matrix( colMeans( sample ) , nrow = ncol( sample ) )
  U = sample - ones( T , 1 ) %*% t( m )
    
  # measure lambdas associated with each observation
  lambdas = diag( U %*% solve( t(U) %*% U ) %*% t(U) ) # result is from Poston, Wegman, Priebe and Solka (1997)
  # the various lambdas denote the t-th element of the diagonal of the information matrix
  # the farthest outlier corresponds to the highest value of lambda
  a = max( lambdas )
  rejected = which.max( lambdas ) # identify the farthest outlier
    
  if ( max(lambdas > 1) == 1 ) { stop( "Lambdas cannot be greater than 1")}
  if ( max(lambdas < 0) == 1 ) { stop( "Lambdas cannot be less than 0")}
    
  return( list( rejected = rejected , lambdas = lambdas ) )
}

#' Compute the minimum volume ellipsoid for a given (multi-variate) time-series
#'
#' Function computes the minimum volume ellipsoid for a given time series
#' 
#' via the expectations-minimization algorithm
#' 
#' \deqn{ w_{t} =  \frac{1}{T} , t = 1,...,T
#' \\ m  \equiv \frac{1}{ \sum_{s=1}^T  w_{s} } \sum_{t=1}^T  w_{t}  x_{t}    
#' \\ S \equiv \sum_{t=1}^T w_{t} \big(x_{t} - m\big) \big(x_{t} - m\big)'
#' \\ Ma_{t}^{2} \equiv  \big(x-m\big)'  S^{-1}  \big(x-m\big), t=1,...,T 
#' \\ w_{t}  \mapsto w_{t} Ma_{t}^{2}
#' \\ U =  \big(x_{1}' -  \hat{E}',...,x_{T}' - \hat{E}' \big)   
#' \\  \hat{Cov}  \equiv  \frac{1}{T} U'U }
#' 
#' The location and scatter parameters that define the ellipsoid are 
#' multivariate high-breakdown estimators of location and scatter 
#' 
#' @param  data     a matrix time-series of data. Each row is a observation (date). Each column is an asset
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @return list     a list with
#'      MVE_Location   a numeric with the location parameter of minimum volume ellipsoid
#'      MVE_Dispersion a numeric with the covariance matrix of the minimum volume ellipsoid
#' 
#' @references 
#' \url{http://www.symmys.com/sites/default/files/Risk\%20and\%20Asset\%20Allocation\%20-\%20Springer\%20Quantitative\%20Finance\%20-\%20Estimation.pdf}
#' See Sec. 4.6.1 of "Risk and Asset Allocation" - Springer (2005), by A. Meucci
#' for the theory and the routine implemented below
#' See Meucci script for "ComputeMVE.m"
#' @export
#' 
ComputeMVE = function ( data )
{
  library( matlab )    
  NumObservations = nrow( data )
  Ones = ones( NumObservations , 1 )    
  det_S_New = 0    
    
  # step 0: initialize the relative weights
  w = 1 / NumObservations * Ones
    
  # step 1: compute the location parameter (m), and the scatter matrix (S)
  m = matrix( colMeans( data ) , nrow = ncol( data ) )
  S = cov( data ) # notice the weights in the scatter matrix are not normalized
    
  keeploop = TRUE
  while ( keeploop == TRUE )
  {
    Mahalanobis = matrix( , ncol = 0 , nrow = 1 )          
    for ( t in 1:NumObservations )
    {
      # cycle thru each observation...
      x_t = t( data[ t , , drop = FALSE ] )                            
            
      # ...and calculate the square Mahalanobis distances
      Mahalanobis = cbind( Mahalanobis , t((x_t - m)) %*% solve(S) %*% ( x_t - m ) )
    }                
    # Step 3: update the weights if the Mahalanobis distance squared > 1 as follows...
    update = matlab:::find(Mahalanobis > 1 )        
    w[ update ] = w[ update ] * t( Mahalanobis[ update ] ) 
    # ... otherwise leave the weight unchanged
        
    # Step 4: If convergence is reached, stop and define mu and Sigma, otherwise go to Step 1
    m = t( data ) %*% w / sum (w )
    S = t(( data - Ones %*% t( m ) )) %*% diag( as.vector( w ) ) %*% ( data - Ones %*% t(m) )
        
    det_S_Old = det_S_New        
    det_S_New = base:::det(S)          
        
    exitFlag = ( ( det_S_Old / det_S_New ) < .99999 ) # loop exits when evaluates to FALSE
        
    if ( det_S_New == 0 ) { exitFlag = TRUE }
        
    if ( !is.logical( exitFlag ) ) { browser() }
        
    keeploop = ( exitFlag )
  }    
    
  MVE_Location = m
  MVE_Dispersion = S
    
  return( list( MVE_Location = MVE_Location , MVE_Dispersion = MVE_Dispersion ) )
}

#' Use the minimum volume ellipsoid to detect outliers
#'
#' See Sec. 4.6.1 of "Risk and Asset Allocation" - Springer (2005), by A. Meucci
#' for the theory and the routine implemented below
#'
#' @param    corruptSample  a matrix of returns with outlier data. Rows are observations, columns are assets.
#'
#' @return   a list containing:
#'      plotdata     a matrix of data used to plot minimum volume ellipsoid as a function of its length
#'      cutofflist   an ordering of observations with the highest Mahalanobis distance (i.e. ordering of outliers by their index )#' 
#'      numOutliers  returns the number of outliers based on the slope of the minimum volume ellipsoid as a function of sample data
#'
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @references 
#' \url{http://www.symmys.com}
#' See Meucci script for "S_HighBreakdownMVE.m"
#' @export
DetectOutliersViaMVE = function( corruptSample )
{
  library( matlab )    
    
  # parameter checks
  if ( ncol( corruptSample ) > nrow( corruptSample ) ) { stop("The number of assets must be greater than number of observations (otherwise system is singular)") }        
    
  # initialize variables
  T = nrow( corruptSample )    
  index = seq( from = 1 , to = T , 1 )
  vol_MVE = sample_Length = matrix( , nrow = 1 , ncol = 0 )
  store = rep( list() , length = ceil( T / 2 ) )
    
  lambdas = RejectOutlier( corruptSample )$lambdas
  cutofflist = order( lambdas , decreasing = TRUE )
    
  # compute high-breakdown estimates
  for ( j in 1:ceil( T / 2 ) ) # stop when the number of data left is less than half the original sample, otherwise repeat
  {    
    # step 1: compute MVE location and dispersion 
    MVE = ComputeMVE( corruptSample )
        
    # store results
    store[[j]]$MVE_Location = MVE$MVE_Location
    store[[j]]$MVE_Dispersion = MVE$MVE_Dispersion
        
    sample_Length = cbind( sample_Length , nrow( corruptSample ) )
    vol_MVE = cbind( vol_MVE , sqrt( det( MVE$MVE_Dispersion ) ) )
        
    store[[j]]$index = index
        
    # step 2: find the farthest outlier among the data and remove the observation (one at a time)        
    rejected = RejectOutlier( corruptSample )$rejected
    corruptSample = corruptSample[ -rejected  , ] # drop outlier rows from sample
    index = index[ -rejected ]
  }
    
  # plot volume of ellipsoid as function of sample length    
  # shows an abrupt jump when the first outlier is added to the data
  # The respective sample covariance is the minimum covariance determinant and the respective ellipsoid
  plot( sample_Length , vol_MVE , type = "l" , main = "Outlier detection" , ylab = "volume of Min volume ellipsoid" , xlab = "sample length" )
    
  # return the minimum volume ellipsoid at each sample length
  result = rbind( sample_Length , vol_MVE )
  rownames( result ) = c("index" , "volumeMVE" )   
    
  # TODO: measure the slope of result$volumeMVE. When slope doubles that marks the first outlier
  numOutliers = NULL
    
  return( list( plotdata = t( result ) , cutofflist = cutofflist , numOutliers = numOutliers ) )
}


#' Generate observations from a two asset covariance matrix and add outliers
#'
#' Generate observations from a covariance matrix and add outliers
#'
#' @param    numGoodSamples      number of observations drawn from the covariance matrix
#' @param    numOutliers         number of outliers added to sample
#' @param    covarianceMatrix    the covariance matrix for the asset returns from which good samples will be drawn
#' @param    shuffle             a boolean suggesting whether order of the twos should be shuffled
#'
#' @return   sample           a matrix of returns consisting of good and bad sample. Rows are observations, columns are the assets.
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
NoisyObservations = function( numGoodSamples , numOutliers , covarianceMatrix , shuffle = FALSE )
{    
  mu = matrix( rep( 0 , nrow( covarianceMatrix ) ) )    
  T = numGoodSamples
    
  # "good" observations
  library( MASS )
  sample = mvrnorm( n = T , mu = mu , Sigma = covarianceMatrix ) # draw T samples from mvrnorm distribution
    
  # generate outliers
  outliers = 10 * matrix( runif( numOutliers * nrow( covarianceMatrix ) ) , nrow = numOutliers , ncol = ncol( covarianceMatrix ) )
    
  # add "bad" observation(s)
  corruptSample = rbind ( sample , outliers )
    
  # shuffle rows
  if ( shuffle == TRUE ) { corruptSample = corruptSample[ sample( nrow( corruptSample ) ) , ] }
    
  return( corruptSample = corruptSample )
}
