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
#' @param nu_post            a numeric with the relative confidence in the prior vs. the sample data. A value of 2 indicates twice as much weight to assign to the prior vs. the sample data. Must be greater than or equal to zero
#' @param riskAversionMu     risk aversion coefficient for estimation of means. 
#' @param riskAversionSigma  risk aversion coefficient for estimation of Sigma.
#' @param discretizations    an integer with the number of portfolios to generate along efficient frontier (equally distanced in return space). Parameter must be an integer greater or equal to 1.
#' @param longonly           a boolean for suggesting whether an asset in a portfolio can be shorted or not
#' @param volatility         a numeric with the volatility used to calculate gamma-m. gamma-m acts as a constraint on the maximum volatility of the robust portfolio. A higher volatility means a higher volatile robust portfolio may be identified.
#'
#' @return a list of portfolios along the frontier from least risky to most risky
#'   bayesianFrontier        a list with portfolio along the Bayesian efficient frontier. Specifically:
#'                               returns: the expected returns of each portfolio along the Bayesian efficient frontier
#'                               volatility: the expected volatility of each portfolio along the Bayesian efficient frontier
#'                               weights: the weights of each portfolio along the Bayesian efficient frontier
#'   robustPortfolio         the most robust portfolio along the Bayesian efficient frontier. Specifically:
#'                               returns: the expected returns of each portfolio along the Bayesian efficient frontier
#'                               volatility: the expected volatility of each portfolio along the Bayesian efficient frontier
#'                               weights: the weights of each portfolio along the Bayesian efficient frontier
#'
#' \deqn{ w_{rB}^{(i)} = argmax_{w \in C, w' \Sigma_{1} w \leq  \gamma_{\Sigma}^{(i)} }  \big\{w' \mu^{1} -  \gamma _{\mu}  \sqrt{w' \Sigma_{1} w} \big\},
#' \gamma_{\mu} \equiv  \sqrt{ \frac{q_{\mu}^{2}}{T_{1}}  \frac{v_{1}}{v_{1} - 2} }
#' \gamma_{\Sigma}^{(i)} \equiv  \frac{v^{(i)}}{ \frac{ \nu_{1}}{\nu_{1}+N+1} + \sqrt{ \frac{2\nu_{1}^{2}q_{\Sigma}^{2}}{ (\nu_{1}+N+1)^{3} } } } }
#' @references
#' A. Meucci - Robust Bayesian Allocation - See formula (19) - (21) 
#' \url{ http://papers.ssrn.com/sol3/papers.cfm?abstract_id=681553 }
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
#' \deqn{ T_{1}  \equiv T_{0} + T
#' \\ \mu_{1}  \equiv \frac{1}{ T_{1} }  \big( T_{0}   \mu_{0}  + T \hat{ \mu } \big)  
#' \\ \nu_{1}  \equiv \nu_{0} + T
#' \\ \Sigma_{1}  \equiv  \big(  \nu_{0} \Sigma_{0}  + T \hat{ \Sigma } +  \frac{ \big(\mu_{0}  - \hat{\mu} \big) \big(\mu_{0}  - \hat{\mu} \big)' }{ \big( \frac{1}{T} +  \frac{1}{T_{0} }  \big) } }
#' @param mean_sample                   the mean of the sample returns
#' @param cov_sample                    the sample covariance matrix
#' @param mean_prior                    the prior for the mean returns
#' @param cov_prior                     the covariance matrix prior
#' @param relativeConfidenceInMeanPrior a numeric with the relative confidence in the mean prior vs. the sample mean. A value of 2 indicates twice as much weight to assign to the prior vs. the sample data. Must be greater than or equal to zero
#' @param relativeConfidenceInCovPrior  a numeric with the relative confidence in the covariance prior vs. the sample covariance. A value of 2 indicates twice as much weight to assign to the prior vs. the sample data. Must be greater than or equal to zero
#' @param sampleSize                    a numeric with the number of rows in the sample data used to estimate mean_sample and cov_sample
#'
#' @return mean_post            a vector with the confidence weighted posterior mean vector of asset returns blended from the prior and sample mean vector
#' @return cov_post             a covariance matrix the confidence weighted posterior covariance matrix of asset returns blended from the prior and sample covariance matrix
#' @return time_post            a numeric
#' @return nu_pst               a numeric
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
#' @references
#' A. Meucci - Robust Bayesian Allocation - See formula (11) - (14) 
#' \url{ http://papers.ssrn.com/sol3/papers.cfm?abstract_id=681553 }
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