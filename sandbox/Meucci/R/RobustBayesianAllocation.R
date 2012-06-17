library( matlab )
library( quadprog )
library( ggplot2 )
library( MASS )

#' Construct the mean-variance efficient frontier using a quadratic solver
#' 
#' Construct a number of long-only or long-short portfolios on the mean-variance efficient frontier where each
#' portfolio is equally distanced in return space
#' @param discretizations   number of portfolios to generate along efficient frontier (where each portfolio is equally distanced in return spaced)
#' @param cov               arithmetic covariance matrix of asset returns
#' @param mu                a vector of arithmetic returns for each asset
#' @param longonly          a boolean which constrains weights to > 0 if true
#'
#' @return a list of portfolios along the frontier from least risky to most risky
#'    The indices in each list correspond to each other
#'      returns             the expected portfolio returns along the frontier
#'      volatility          the variance of the portfolio along the frontier
#'      weights             the weights of the portfolio components along the frontier
#' @references  Attilio Meucci, 2011, Robust Bayesian Allocation 
#' \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=681553}
#' @seealso \url{http://symmys.com/node/102}
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
efficientFrontier = function( discretizations , cov , mu , longonly = FALSE ) 
{    
  # setup quadratic program
  N = nrow( cov )
  firstDegree = zeros( N , 1 )
  secondDegree = cov
  Aeq = ones( 1 , N ) ; beq = 1
  A = eye( N )
  b = zeros( N , 1 )
  
  if ( !longonly )
    { Aqp = t( Aeq ) ; bqp = beq } 
  else
    { Aqp = t( rbind( Aeq , A ) ) ; bqp = c( beq , b ) }
  
  # determine return of minimum-risk portfolio
  minVolWeights = solve.QP( secondDegree , firstDegree , Aqp , bqp , length( beq ) )$solution
  minVolRet = minVolWeights %*% mu
  
  # determine return of maximum-return portfolio
  maxRet = max( mu )
  
  # slice efficient frontier in 'discretizations' number of equally thick horizontal sectors in the upper branch only
  step = ( maxRet - minVolRet ) / ( discretizations - 1 )
  targetReturns = seq( minVolRet , maxRet , step )
  
  # compute the compositions and risk-return coordinates of the optimal allocations relative to each slice
  
  # start with min vol portfolio
  weights = minVolWeights
  volatility = sqrt( minVolWeights %*% cov %*% minVolWeights )
  returns = minVolRet
  
  for( i in 2:( discretizations - 1 ) ){
    #  determine least risky portfolio for given expected return
    Aeq = ones( 1 , N )
    Aeq = rbind( Aeq , t( mu ) )
    
    beq = c( 1 , targetReturns[i] )
    if( !longonly ){
      Aqp = t( Aeq ) #combine A matrices
      bqp = beq #combine b vectors
    }else{
      Aqp = t( rbind( Aeq , A ))
      bqp = c(beq,b)
    }
    
    solvedWeights = solve.QP( secondDegree , firstDegree , Aqp , bqp , 1 )$solution
    weights = rbind( weights , solvedWeights )
    volatility = c( volatility , sqrt( solvedWeights %*% cov %*% solvedWeights ) )
    returns = c( returns , solvedWeights %*% mu )
    
  }  
  return( list( returns = returns , volatility = volatility , weights = weights ) )
}


#' Construct a Bayesian mean-variance efficient frontier and identifies the most robust portfolio
#'
#' Construct a collection of portfolios along the Bayesian mean-variance efficient frontier
#' where each portfolio is equally distanced in return space. The function also returns the most robust
#' portfolio along the Bayesian efficient frontier
#'
#' @param mean_post          the posterior vector of means (after blending prior and sample data)
#' @param cov_post           the posterior covariance matrix (after blending prior and sample data)
#' @param confidenceInPrior  a numeric with the relative confidence in the prior vs. the sample data. A value of 2 indicates twice as much weight to assign to the prior vs. the sample data. Must be greater than or equal to zero
#' @param riskAversionMu     risk aversion coefficient for estimation of means. 
#' @param riskAversionSigma  risk aversion coefficient for estimation of Sigma.
#' @param discretizations    an integer with the number of portfolios to generate along efficient frontier (equally distanced in return space). Parameter must be an integer greater or equal to 1.
#' @param volatility         a numeric with the volatility used to calculate gamma-m. gamma-m acts as a constraint on the maximum volatility of the robust portfolio. A higher volatility means a higher volatile robust portfolio may be identified.
#'
#' @return a list of portfolios along the frontier from least risky to most risky
#'   bayesianFrontier        a list with portfolio along the Bayesian efficient frontier. Specifically:
#'                               returns: the expected returns of each portfolo along the Bayesian efficient frontier
#'                               volatility: the expected volatility of each portfolo along the Bayesian efficient frontier
#'                               weights: the weights of each portfolo along the Bayesian efficient frontier
#'   robustPortfolio         the most robust portfolio along the Bayesian efficient frontier. Specifically:
#'                               returns: the expected returns of each portfolo along the Bayesian efficient frontier
#'                               volatility: the expected volatility of each portfolo along the Bayesian efficient frontier
#'                               weights: the weights of each portfolo along the Bayesian efficient frontier
#'
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
robustBayesianPortfolioOptimization = function( mean_post , cov_post , nu_post , riskAversionMu = .1 , riskAversionSigma = .1 , discretizations = 10 , longonly = FALSE , volatility )
{    
  # parameter checks    
  N = length( mean ) # number of assets    
  if ( ( N < 2 ) == TRUE ) { stop( "Requires a minimum of two assets to perform optimization" ) }
  if ( discretizations < 1 ) { stop( "Number of discretizations must be an integer greater than 1" ) }  
  if ( volatility < 0 ) { stop( "Volatility cannot be a negative number" ) }
  if ( nu_post < 3 ) { stop( "nu_post must be greater than 2 otherwise g_m is undefined " ) }
  if ( riskAversionMu < 0 ) { stop( "riskAversionMu must be a positive number" ) }
  if ( riskAversionSigma < 0 ) { stop( "riskAversionSigma must be a positive number" ) }
    
  # construct Bayesian efficient frontier
  bayesianFrontier = efficientFrontier( discretizations , cov_post , mean_post , longonly = TRUE ) # returns a list of returns, volatility, and assets weights along the posterior frontier. Each row represents a point on the frontier
  
  # measure gamma-m and gamma-s to identify which portfolios along the frontier are robust
  quantileMeanSquared = qchisq( riskAversionMu , N ) # the value of q-u is typically set to the quantile of the chi-squared distribution with N degrees of freedom (formula 6)    
        
  # g_m is defined as a constraint on the optimal robust portfolio such that the variance of the robust portfolio must be less than gamma-m
  g_m = sqrt( quantileMeanSquared / time_post * nu_post / ( nu_post - 2 ) ) # gamma-m (formula 20)
        
  quantileCovSquared = qchisq( riskAversionSigma , N * ( N + 1 ) / 2 ) # from formula 7. N*(N+1)/2 is the degrees of freedom in a symmetric matrix (number of unique elements)        
  g_s = volatility / ( nu_post / ( nu_post + N + 1 ) + sqrt( 2 * nu_post * nu_post * quantileCovSquared / ( ( nu_post + N + 1 ) ^ 3 ) ) ) # gamma-sigma (formula 21) corresponding to the i'th portfolio along the sample efficient frontier
    
  # initialize parameters
  target = NULL

  # for each of the portfolios along the efficient Bayesian frontier identify the most robust portfolio
  for( k in 1:( discretizations - 1 ) ) 
  {                
    weightsBay = bayesianFrontier[[ "weights" ]][ k , ]                
        
    # reject portfolios that do not satisfy the constraints of formula 19 (i.e. Bayesian portfolios that are not robust, for example, the portfolios at the limit -- 100% confidence in prior or 100% confidence in sample)        
    # identify Robust Bayesian frontier which is a subset of the Bayesian frontier that is further shrunk to toward the global minimumm variance portfolio
    # and even more closely tight to the right of the efficient frontier        
    if ( weightsBay %*% cov_post %*% weightsBay <= g_s ) # constraint for formula 19
    { target = c( target , weightsBay %*% mean_post - g_m * sqrt( weightsBay %*% cov_post %*% weightsBay )) } # formula 19
    else { target = c( target , -999999999 ) } # if the Bayesian efficient portfolio does not satisfy the constraint we assign a large negative value (we will reject these portfolios in the next step)
  }    

  maxTarget = max( target )    
  if ( maxTarget == -999999999 ) { stop( "No robust portfolio found within credibility set. Try increasing volatility or adjusting risk aversion parameters." ) }
  maxIndex = which( target == maxTarget , arr.ind = TRUE ) # identify most robust Bayesian portfolio
  if ( length( maxIndex ) > 1 ) { stop( "The number of robust portfolios identified is greater than 1. Debug. " )}
    
  # identify Robust portfolio as a subset of Bayesian frontier    
  robustPortfolio = list( returns    = bayesianFrontier[[ "returns" ]][ maxIndex ] ,
                          volatility = bayesianFrontier[[ "volatility" ]][ maxIndex ] ,
                          weights    = bayesianFrontier[[ "weights" ]][ maxIndex , ] )    
    
  return( list( bayesianFrontier = bayesianFrontier , robustPortfolio = robustPortfolio , g_m = g_m , g_s = g_s ) )
    
  # Test that the number of returns portfolios is <= number of discretizations
  # Test that there are no NA's in the return results
}

# Example:
    # robustBayesianPortfolioOptimization( mean_post = mean_post , cov_post = cov_post , nu_post = 156 , riskAversionMu = .1 , riskAversionSigma = .1 , discretizations = 10 , longonly = TRUE , volatility = .10 )

#' Constructs the partial confidence posterior based on a prior, sample mu/covariance, and relative confidence in the prior
#'
#' Constructs the partial confidence posterior based on prior (mean vector and covariance matrix) and a posterior
#' with a relative confidence in the prior vs. the sample data
#'
#' @param mean                  the mean of the sample returns
#' @param cov                   the sample covariance matrix
#' @param mean_prior            the prior for the mean returns
#' @param cov_prior             the covariance matrix prior
#' @param confidenceInMeanPrior a numeric with the relative confidence in the mean prior vs. the sample mean. A value of 2 indicates twice as much weight to assign to the prior vs. the sample data. Must be greater than or equal to zero
#' @param confidenceInCovPrior  a numeric with the relative confidence in the covariance prior vs. the sample covariance. A value of 2 indicates twice as much weight to assign to the prior vs. the sample data. Must be greater than or equal to zero
#' @param sampleSize            a numeric with the number of rows in the sample data used to estimate mean_sample and cov_sample
#'
#' @return mean_post            a vector with the confidence weighted posterior mean vector of asset returns blended from the prior and sample mean vector
#' @return cov_post             a covariance matrix the confidence weighted posterior covariance matrix of asset returns blended from the prior and sample covariance matrix
#' @return time_post            a numeric with ... TODO: 
#' @return nu_pst               a numeric with ... TODO:
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
PartialConfidencePosterior = function( mean_sample , cov_sample , mean_prior , cov_prior , relativeConfidenceInMeanPrior , relativeConfidenceInCovPrior , sampleSize )
{
  # parameter checks
  if ( (length( mean_sample ) == nrow( cov_sample )) == FALSE ) { stop( "number of assets in mean must match number of assets in covariance matrix")}
  if ( (length( mean_sample ) == length( mean_prior )) == FALSE ) { stop( "number of assets in mean must match number of assets in mean_prior")}
  if ( ( nrow( cov_sample ) == nrow( cov_prior ) ) == FALSE ) { stop( "number of assets in sample covariance must match number of assets in prior covariance matrix")}
  N = length( mean_sample ) # number of assets    
  if ( ( N < 2 ) == TRUE ) { stop( "requires a minimum of two assets to perform optimization" ) }
  if ( relativeConfidenceInMeanPrior < 0 ) { stop( "Confidence in mean prior must be a number greater than or equal to zero" ) }
  if ( relativeConfidenceInCovPrior  < 0 ) { stop( "Confidence in covariance prior must be a number greater than or equal to zero" ) }
    
  # Investor's experience and confidence is summarized by mean_prior, cov_prior, time_prior, and nu_prior
  # nu_prior = confidence on the inverse of cov_prior (see 7.25 - Meucci Risk & Asset Allocation Text). A larger value of nu_prior corresponds to little uncertainty about the view on inverse of Sigma, and thus Sigma    
  # confidenceInPrior = time_prior = T0 = confidence in the prior view mean_prior
  confidenceInSample = sampleSize # typically the number of observations on which the mean_sample and cov_sample is based on
  confidenceInMeanPrior = sampleSize * relativeConfidenceInMeanPrior
  confidenceInCovPrior = sampleSize * relativeConfidenceInCovPrior
    
  # blend prior and the sample data to construct posterior
  time_post = confidenceInSample + confidenceInMeanPrior
  nu_post = confidenceInSample + confidenceInCovPrior    
  mean_post = 1/time_post * ( mean_sample * confidenceInSample + mean_prior * confidenceInMeanPrior )    
  cov_post = 1/nu_post * (cov_sample * confidenceInSample + cov_prior * confidenceInCovPrior + ( mean_sample - mean_prior ) %*% t( ( mean_sample - mean_prior ) ) / ( 1 / confidenceInSample + 1 / confidenceInMeanPrior ) )
    
  return( list( mean_post = mean_post , cov_post = cov_post , time_post = time_post , nu_post = nu_post ) )    
    
  # TODO: Test expectations
    # Test 1: If relative confidence in prior is 0, then returns mean_sample and cov_sample
    # Test 2: If relative confidence in prior is 1, and sampleSize = 0 then returns mean_prior and cov_prior
    # Test 3: As the number of sample size observations increase, the posterior mean and covariance shrinks toward mean_sample and cov_sample
}

####################################################################
# Example from Meucci's MATLAB script:  S_SimulationsCaseStudy.M
# See MATLAB package "Meucci_RobustBayesian" for original MATLAB
# source on www.symmys.com
####################################################################

####################################################################
# inputs
####################################################################
J = 50 # number of simulations
T = 52 # number of observations in time series
N = 20 # number of assets in the market
r = .4 # overall correlation in constant correlation matrix
min_s = .1 # min volatility among the N assets
max_s = .4 # max volatility among the N assets
NumPortf = 10 # number of discretizations for efficient frontier
p_m = .1 # aversion to estimation risk for mu
p_s = .1 # aversion to estimation risk for sigma

####################################################################
# true market parameters
####################################################################
C = ( 1 - r ) * eye( N ) + r * ones( N , N ) # creates a homogenous correlation matrix
step_s = ( max_s - min_s ) / ( N - 1 ) # 1st asset will have min volatility...
s = seq( min_s , max_s , step_s ) # ... last asset will have maximum volatility
S = diag(s) %*% C %*% diag(s) # fake covariance matrix with equally spaced volatilities

# Note the means are defined in such a way that a mean-variance optimization would yield an equally weighted portfolio
# fake mean matrix : mus = 2.5 * Sigma / N
M = 2.5 * S %*% ones( N , 1 ) / N

####################################################################
# conduct Monte carlo simulation
####################################################################

# initialize variables
meanVarMus = meanVarVols = trueMus = trueVols = bayesianMus = bayesianVols = robustMus = robustVols = list()

# construct efficient sample, bayesian, and robust bayesian frontier for each simulation
for( j in 1:J )
{
  # Sample T draws from the true covariance matrix
  rets = mvrnorm( T , M , S ) 
  
  # construct mean-variance frontier using sample estimate.
  mean = colMeans( rets ) # get mean vector
  cov = cov( rets ) # cov vector  
  sampleFrontier = efficientFrontier( NumPortf , cov , mean , TRUE ) # returns a list of returns, volatility, and assets weights along the frontier. Each row represents a point on the frontier
  
  # construct mean-variance efficient portfolio based on true Mu and sigma  
  trueFrontier = efficientFrontier( NumPortf , S , M , TRUE ) 
  
  # Bayesian prior for covariance and mu's (an arbitrary prior model of covariance and returns)
  # the covariance prior is equal to the sample covariance on the principal diagonal
  cov_prior  = diag( diag( cov ) ) 
  
  # set the prior expected returns for each asset to : mus = .5 * Sigma(1/N). Incidentally, this ensures there is a perfect positive linear relationship between asset variance and asset expected  return
  mean_prior = .5 * cov_prior %*% rep( 1/N , N ) 
  
  # set the confidence in the prior as twice the confidence in the sample and blend the prior with the sample data
  posterior = PartialConfidencePosterior( mean_sample = mean , cov_sample = cov , mean_prior = mean_prior , cov_prior = cov_prior , 
                                            relativeConfidenceInMeanPrior = 2 , relativeConfidenceInCovPrior = 2 , sampleSize = nrow( rets ) )

  cov_post = posterior$cov_post ; mean_post = posterior$mean_post ; time_post = posterior$time_post ; nu_post = posterior$nu_post ; rm( posterior )
  
  # construct Bayesian frontier using blended mu and Sigma, and identify robust portfolio
  # returns a set of Bayesian efficient portfolios: a list of returns, volatility, and assets weights along the posterior frontier. Each row represents a point on the frontier
  # and the returns, volatility, and assets of the most robust portfolio in the set
  
  pickVol = round( .8 * NumPortf ) # Picks the 80% highest volatility ( a parameter )...
  volatility = ( sampleFrontier[[ "volatility" ]][ pickVol ] ) ^ 2 # ...on the sample *efficient* frontier. On the efficient *sample* frontier. This is why the problem is a second-order cone programming problem. TODO: why use sample frontier?
  
  if ( is.na(volatility) == TRUE ) { stop( "The chosen volatility is too high" ) }
  
  frontierResults = robustBayesianPortfolioOptimization( mean_post = mean_post , 
                                                         cov_post = cov_post , 
                                                         nu_post = nu_post ,
                                                         riskAversionMu = p_m , 
                                                         riskAversionSigma = p_s , 
                                                         discretizations = NumPortf ,
                                                         longonly = FALSE ,
                                                         volatility = volatility )
  
  bayesianFrontier = frontierResults$bayesianFrontier ; robustPortfolio = frontierResults$robustPortfolio ; rm(frontierResults) 
  
  # initialize parameters
  mumv = volmv = mutrue = voltrue = mubay = volbay = NULL
  
  # for each  portfolios along the sample and Bayesian frontiers, measure the actual returns and actual volatility based on the true returns/true covariance
  for( k in 1:( NumPortf - 1 ) ) 
  {      
    # Notice when plotting the sample-based allocation is broadly scattered in inefficient regions
    weightsMV = sampleFrontier[[ "weights" ]][ k , ] # retrieve the weights assigned to the k'th portfolio along the sample frontier
    mumv = c( mumv , weightsMV %*% M ) # given the optimal weights from sample estimates of mu and Sigma, measure the actual return using the true asset means
    volmv = c( volmv , ( weightsMV %*% S %*% weightsMV ) ) # given the optimal weights from the sample estimates of mu and Sigma, measure the actual variance of the portfolio
    
    # measure actual performance using true mu and cov
    weightsMVTrue = trueFrontier[[ "weights" ]][ k , ] # retrieve the weights assigned to the k'th portfolio along the true frontier
    mutrue = c( mutrue , weightsMVTrue %*% M ) # given the optimal weights from actual values of mu and Sigma, measure the actual return using the true asset means
    voltrue = c( voltrue , ( weightsMVTrue %*% S %*% weightsMVTrue ) ) # given the optimal weights from the sample estimates of mu and Sigma, measure the actual variance of the portfolio    

    weightsBay = bayesianFrontier[[ "weights" ]][ k , ]
    mubay = c( mubay , weightsBay %*% M ) # given the optimal weights from Bayesian estimates of mu and Sigma, measure the actual return using the true asset means
    volbay = c( volbay , ( weightsBay %*% S %*% weightsBay ) ) # given the optimal weights from the Bayesian estimates of mu and Sigma, measure the actual variance of the portfolio
  }   
    
  # measure the actual performance of the most Robust portfolio along the Bayesian efficient frontier
  weightsRob = robustPortfolio$weights
  murob = weightsRob %*% M
  volrob = weightsRob %*% S %*% weightsRob
     
  # collect actual return and actual variances results for each portfolio in each simulation
  meanVarMus[[ j ]] = mumv # list of actual returns along efficient frontier for each simulation based on portfolio constructed using sample mean and sample co-variance
  meanVarVols[[ j ]] = volmv # ...and the list of actual variances along efficient frontier
  bayesianMus[[ j ]] = mubay # list of actual returns based on bayesian mixing of prior and data sampled from true distribution
  bayesianVols[[ j ]] = volbay # ...and the list of associated actual variances
  robustMus[[ j ]] = murob # list of actual return based on robust allocation... Note only one robust portfolio per simulation j is identified.
  robustVols[[ j ]] = volrob # ...and the list of associated actual variances. Note only one robust portfolio per simulation j is identified.
  trueMus[[ j ]] = mutrue # list of actual return based on optimizing with the true mus...
  trueVols[[ j ]] = voltrue # ...and the list of associated actual variances  
}

# Plot sample, bayesian, and robust mean/variance portfolios

# create dataframe consisting of actual returns, actual variance, and sample indicator    
actualReturns = unlist( meanVarMus ) ; actualVariance = unlist( meanVarVols )
plotData1 = data.frame( actualReturns = actualReturns, actualVariance = actualVariance , type = "Sample" )
actualReturns = unlist( bayesianMus ) ; actualVariance = unlist( bayesianVols )
plotData2 = data.frame( actualReturns = actualReturns, actualVariance = actualVariance , type = "Bayesian" )
actualReturns = unlist( robustMus ) ; actualVariance = unlist( robustVols )
plotData3 = data.frame( actualReturns = actualReturns, actualVariance = actualVariance , type = "Robust Bayesian" )
actualReturns = unlist( trueMus ) ; actualVariance = unlist( trueVols )
plotData4 = data.frame( actualReturns = actualReturns, actualVariance = actualVariance , type = "True frontier" )
plotData = rbind( plotData1 , plotData2 , plotData3 , plotData4 ) ; rm( plotData1 , plotData2 , plotData3 , actualReturns , actualVariance )

# build plot with overlays    
# Notice when plotting the the Bayesian portfolios are shrunk toward the prior. Therefore they 
# are less scattered and more efficient, although the prior differs significantly from the true market parameters.
ggplot( data = plotData ) + geom_point( aes_string( x = "actualVariance" , y = "actualReturns" , color = "type"  ) )
