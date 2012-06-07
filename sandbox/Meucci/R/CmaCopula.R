
# fix documentation for @return in PanicCopula()
# plot shape of the copula (perspective or surface plot)
# fix MvnRnd function (Schur decomposition)
# fix warnings

#' Generates normal simulations whose sample moments match the population moments
#'
#' Adapted from file 'MvnRnd.m'. Most recent version of article and code available at http://www.symmys.com/node/162
#' see A. Meucci - "Simulations with Exact Means and Covariances", Risk, July 2009
#'
#' @param M         a numeric indicating the sample first moment of the distribution
#' @param S         a covariance matrix
#' @param J         a numeric indicating the number of trials
#'
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' @references 
#' \url{http://www.symmys.com}
#' TODO: Add Schur decomposition. Right now function is only sampling from mvrnorm so sample moments do no match population moments
#' I have sample code commented out below to implement this correctly but I require a function that returns the unitaryMatrix from a Schur decomposition
MvnRnd = function( M , S , J )
{
    library(MASS)
    X = MASS::mvrnorm( n = J , mu = M , Sigma = S ) # Todo: need to swap with Meucci function and Schur method
    return( X = X )
    
    # # compute sample covariance: NOTE defined as "cov(Y,1)", not as "cov(Y)"
    # S_ = cov( Y , 1 )
    # 
    # # solve Riccati equation using Schur method
    #     zerosMatrix = matrix( rep( 0 , length( N * N ) ) , nrow = N )
    #     # define the Hamiltonian matrix
    #     H1 = cbind( zerosMatrix , -1*S_ )
    #     H2 = cbind( -S , zerosMatrix ) 
    #     H = rbind( H1 , H2 )
    #     # perform its Schur decomposition. 
    #     # TODO: check that the result returns real eigenvalues on the diagonal. ?Schur seems to give an example with real eigenvalues
    #     schurDecomp = Schur( H )
    #     T = SchurDecomp
    #     # U_ = unitaryMatrix??? TODO: Find a function in R that returns the unitaryMatrix from a Schur decomposition
    #     # U = ordschur(U_,T_,'lhp')
    #     # U_lu = U(1:N,1:N)
    #     # U_ld = U(N+1:end,1:N)
    #     # B = U_ld/U_lu
    # 
    # # affine transformation to match mean and covariances
    # # X = Y%*%B + repmat(M', J , 1 ) 
    #
}


#' CMA separation. Decomposes arbitrary joint distributions (scenario-probabilities) into their copula and marginals
#'
#' The CMA separation step attains from the cdf "F" for the marginal "X", the scenario-probabilities representation 
#'      of the copula (cdf of U: "F") and the inter/extrapolation representation of the marginal CDF's
#'
#' Separation step of Copula-Marginal Algorithm (CMA)
#' Meucci A., "New Breed of Copulas for Risk and Portfolio  Management", Risk, September 2011
#' Most recent version of article and code available at http://www.symmys.com/node/335
#'
#' @param   X    A matrix where each row corresponds to a scenario/sample from a joint distribution. 
#'                    Each column represents the value from a marginal distribution
#' @param   p    A 1-column matrix of probabilities of the Jth-scenario joint distribution in X
#'
#' @return  xdd  a JxN matrix where each column consists of each marginal's generic x values in ascending order
#' @return  udd  a JxN matrix containing the cumulative probability (cdf) for each marginal by column - it is rescaled by 'l' to be <1 at the far right of the distribution
#'                  can interpret 'udd' as the probability weighted grade scenarios (see formula 11 in Meucci)
#' @return  U    a copula (J x N matrix) - the joint distribution of grades defined by feeding the original variables X into their respective marginal CDF
#'
#'
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' @references 
#' \url{http://www.symmys.com}
CMAseparation = function( X , p ) 
{
# @example     test = cbind( seq( 102 , 1 , -2 ) , seq( 100 , 500 , 8 ) )
# @example     prob = rep(.02 , 51 )
# @example     CMAseparation( test , prob )
    
    library(pracma)
# preprocess variables
    X = as.matrix( X ) # J x N matrix. One column for each marginal distribution, J scenarios
    p = as.matrix( p ) # J x 1 matrix of twisted probabilities
    J = nrow( X )
    N = ncol( X )
    l = J / ( J + 1 ) # Laplace
    
    # To ensure that no scenario in the prior is strictly impossible under the prior distribution we take the max of the prior or epsilon
    # This is necessary otherwise one cannot apply Bayes rule if the prior probability of a scenario is 0
    p = apply( cbind( p , 1 / J * 10^(-8) ) , 1 , max ) # return a J x 1 matrix where each row is the max of the row in p or 1 / J * 10^(-8)    
    
    p = p / sum(p) # Return a re-scaled vector of probabilities summing to 1
    u = 0 * X # initialize a J x N matrix of 0's
    U = 0 * X
    
# begin core algorithm        
    # initialize empty J x N matrices
    x = matrix( nrow = J , ncol = N ) 
    Indx = matrix( nrow = J , ncol = N )
    
    for ( n in 1:N ) # for each marginal...
    {
        x[ , n ] = sort( X[ , n ] , decreasing = FALSE ) # a JxN matrix where each column consists of each marginal's generic x values in ascending order
        Indx[ , n ] = order( X[ , n ] ) # a JxN matrix. Each column contains the rank of the associated generic X in that column
    }
    
    for ( n in 1:N ) # for each marginal...
    {
        I = Indx[ , n ] # a Jx1 vector consisting of a given marginal's rank/index    
        cum_p = cumsum( p[I] ) # compute cdf. The head is a small number, and the tail value is 1
        u[ , n ] = cum_p * l # 'u' will contain cdf for each marginal by column - it is rescaled by 'l' to be <1 at the far right of the distribution    
        
        # For each marginal distribution, CMA creates from each grid of scenario pairs { x-n,j , u-n,j } a function I{ x-n,j , u-n,j }
        #       that inter/extrapolates those values. See Figure 1 of "Meucci - A New Breed of Copulas for Portfolio and Risk Management"
        Rnk = round( interp1( x = I , y = 1:J , xi = 1:J ) ) # compute ordinal ranking of each entry    #TODO: fix warnings - "Points in argument in 'x' unsorted; will be sorted"
        
        # U is a copula (matrix) - the joint distribution of grades defined by feeding the original variables X into their respective marginal CDF
        U[ , n ] = cum_p[ Rnk ] * l # J x N matrix of grades. compute grades by feeding each column of marginals into its own CDF. U=F(X). Grades can be interepreted as a non-linear z-score. 
    }
    
    return ( list( xdd = x , udd = u , U = U ) )
}

#' CMA combination. Glues an arbitrary copula and arbitrary marginal distributions into a new joint distribution
#'
#' Combination step of Copula-Marginal Algorithm (CMA)  based on Meucci A., "New Breed of Copulas for Risk and Portfolio  Management", Risk, September 2011. Most recent version of article and code available at http://www.symmys.com/node/335
#'
#' @param   x   a generic x variable. Note: Linearly spaced 'x' help for coverage when performing linear interpolation
#' @param   u   The value of the cumulative density function associated with x (parametric or non-parametric)
#' @param   U   an aribtrary copula. Can take any copula obtained with the separation step (i.e. a set of scenario-probabilities)
#'
#' @return  X   a J x N matrix containing the new joint distribution based on the arbitrary copula 'U'
#'
#'
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' @references 
#' \url{http://www.symmys.com}
CMAcombination = function( x , u , U ) 
# @example     test = cbind( seq( 102 , 1 , -2 ) , seq( 100 , 500 , 8 ) )
# @example     prob = rep(.02 , 51 )
# @example     CMAseparation( test , prob )
{
    library(Hmisc)
    x = as.matrix( x ) 
    J = nrow( x )
    K = ncol( x )
    X = as.matrix( 0 * U ) # a J x N zero matrix
    
    for ( k in 1:K )
    {
        # Notation: 'j' is a scenario; 'n' is an asset. x-n,j is a generic value of the marginal distribution for the j'th scenario
        # CMA takes each grade scenario for the copula u-n,j and maps it into the desired combined scenarios x-n,j by inter/extrapolation
        #       of the copula scenarios u-n,j in a manner similar to step (12) but *reversing the axes*. This replaces the computation of
        #       the inverse cdf of F for an aribtrary marginal            
        X[ , k ]  = approxExtrap( x = u[ , k ] , y = x[ , k ] , xout = U[ , k ] , method = "linear" , rule = 2 , ties = ordered )$y # TODO: Is this an inverse CDF? Check out plot( u , y )    
        
        # plot ( u[ , 1] , y[ , 1] ) # to see plot of this inverse cdf for an example
    }    
    return( X )
}

#' Copula-Marginal Algorithm (CMA)
#'
#' Copula-Marginal Algorithm (CMA) to generate and manipulate flexible copulas, as described in
#' Meucci A., "New Breed of Copulas for Risk and Portfolio Management", Risk, September 2011
#' Most recent version of article and code available at http://www.symmys.com/node/335
#'
#' @param   N       number of assets
#' @param   J       number of samples from joint distribution of asset returns
#' @param   r_c     average correlation in a calm market
#' @param   r       average correlation in a panic market
#' @param   b       probability of a panic market occurring
#' @param   sigma   covariance matrix of asset returns
#' @param   sig     TBD
#'
#' @return  a list with:
#'          copula              a couplua
#'          hist                a object of type histogram
#'          p_                  the revised probabilities (invisible)
#'          meanReturn          the mean return for the portfolio given the views
#'          varianceReturn      the variance of the portfolio returns
#' @examples
#' PanicCopula( N = 20 , J = 50000 , r_c = .3 , r = .99 , b = .02 , sig = .2 , sigma )
#' @export
#' @author Ram Ahluwalia \email{rahluwalia@@gmail.com}
#' @references 
#' \url{http://www.symmys.com}
PanicCopula = function( N = 20 , J = 50000 , r_c = .3 , r = .99 , b = .02 , sig = .2 , sigma )
{
# generate panic distribution
    library(matlab)
    library(Matrix)    
    
    # Step 1 of Entropy Pooling: Generate a panel of joint scenarios for the base-case (the prior)
    # Below we construct a J x N panel of independent joint scenarios using the covariance matrix (Sigma)
    # build a matrix of ones ( J x 1 ), and assign equal probability to each row
    # Alternative #2:, if the risk drivers are "invariants", i.e. if they can be assumed independently
    # and identically distributed across time, the scenarios can be historical realizations
    # Alternative #3: the scenarios can be Monte Carlo simulations
    # The collection of joint-scenarios and the J-dimensional vector-p of the probabilities
    # of each scenario (X,p) constitutes the "prior" distribution of the market
    p = ones( J , 1 ) / J 
    
    c2_c = ( 1 - r_c) * diag( N ) + r_c * ones( N , N ) # NxN correlation matrix in a calm market   
    
    c2_p = ( 1 - r ) * diag(N) + r * ones( N , N ) # NxN correlation matrix in a panic market    
    
    # The return disributions are centered on zero, however, the 'panic' correlation has a higher variance
    # By the below construction, the distribution of returns from the 'calm' corr matrix and 'panic' 
    # corr matrix for each row are related
    s2 = as.matrix( bdiag( c2_c , c2_p ) ) # create a symmetric block diagonal matrix of dimension 2N x 2N    
    Z = MvnRnd( zeros( 2*N , 1 ) , s2 , J ) # simulate J draws from s2 matrix with mean zero. A J x 2N matrix  
    X_c = Z[ , 1:N ] # J x N joint distribution in calm market. The joint distribution is somewhat correlated.
    X_p = Z[ , ( N + 1 ):ncol( Z ) ] # J X N joint distribution in panic market. The joint distribution is highly correlated.    
    
    # the bottom b quantile of negative return rows from the panic distribution are identified 
    D = ( pnorm( X_p ) < b ) # Create a J x N logical matrix testing the probability of the joint distribution being less than the threshold 'b'
    
    # The bottom 'b'-quantile of returns from the panic market replaces returns from the calm market creating a bi-model left-skewed distribution
    # 'X'' represents the joint-scenario probabilities including the risk of a panic market
    X = ( (1-D) * X_c ) + ( D * X_p ) 
    
    # Step 2: perturb probabilities via Fully Flexible Views. In particular, formulate stress-tests or views in terms of linear
    # constraints on a yet-to-be defined set of probabilities 'p_'
    # Conceptually, the method leaves the domain of the distribution unaltered and instead shifts the probability masses to reflect the views
    # Notation:
    # Aeq: matrix of equality constraints (denoted as 'H' in the "Meucci - Flexible Views Theory & Practice" paper formlua 86 on page 22)
    # beq: the vector associated with Aeq equality constraints  
    # Define the matrix-vector pair (H,h) corresponding to objects (Aeq, beq) and constrain probabilities to sum to one
    # optimizer is constraining choice of probabilities to Hx = h so fixing the top row of 'H', and top element of 'h' to one
    # forces the probabilites to sum to one: H %*% p = h
    Aeq = ones( 1 , J ) # 1 x J matrix consisting of ones
    beq = 1
    
    # constrain the first moments (i.e. the expected - scenario-probability weighted asset return - for all assets is zero)
    Aeq = rbind( Aeq , t(X) ) # N+1 x J matrix. Top row is all 1's
    beq = as.matrix( rbind( beq , zeros( N , 1 ) ) ) # N+1 x 1 column matrix. 1st element is a 1, rest are zeros                            
    
    # Step 3: Check for consistency of constraints. TODO: Where is this performed?
    
    # Step 4 (performed in EntropyProg): Find 'p_' that display the least relative entropy from the prior distribution and
    # at the same time satisfy the stress-test/view constraints    
    emptyMatrix = matrix(nrow=0, ncol=0) # equivalent to MATLAB expression = [ ]
    EntropyProgResult = EntropyProg( p , emptyMatrix , emptyMatrix , Aeq , beq )
    p_ = EntropyProgResult$p  # the Jx1 matrix of posterior probabilities
    
    # plot the pdf of the revised probabilities (vs. the uniform prior)
    plot( x = seq( 1:J ) , y = sort( p_ ) , type = "l" , xlab = "Observations" , ylab = "Partial Density Function for p_")
    
    # Note: one can validate that the views of asset returns (constraints on first moments) are indeed satisfied
    # Now we calculate summary statistics by taking a p_ weighted average of a pricing function (or mean, variance, etc.) for each scenario
    # Aeq %*% p_ # equals the expectations in beq            
    meanPriorReturnScenarios = apply( Aeq[ -1 , ] , 2 , mean ) # mean return for j'th prior scenario (distribution remains unaltered)
    # meanPriorReturnScenarios %*% p_ # expected portfolio return with new probabilities at each scenario
    breaks = ProbabilityWeightedHistogram( meanPriorReturnScenarios , p_ , round( 10 * log(J)) , freq = FALSE )$Breaks            
    # weighted.hist( meanPriorReturnScenarios, p_ , breaks , freq = FALSE )  # create weighted histogram. TODO: FIX. Cannot abline the expected portfolio return    
    
    bin <- cut( meanPriorReturnScenarios , breaks , include.lowest = TRUE)
    wsums <- tapply( p_ , bin, sum ) # sum the probabilities in each bin
    wsums[is.na(wsums)] <- 0
    prob <- wsums/(diff(breaks) * sum( p_ ))  
    hist( prob , breaks )    
    
    print( c( "Variance of p_" , var( p_ ) ) ) # high variance indicates higher levels of distortion
    print( c( "Max p_ divided by average p" , max(p_) / mean(p_) ) )
    
    # With the new probabilities (weights to joint-scenarios) in hand, one can compute the updated
    # expectation of any other function of interest h(y) as : sum across all scenarios [ p_ * h(y-jth scenario ) ]    
    print( EntropyProgResult$optimizationPerformance )
    
# Extract the marginal cdf's from the distribution represented by the joint scenarios
    # NOTE: An alternative approach proposed to broaden the choice of copulas involves simulating the grade
    # scenarios u-n,j directly from a parametric copula F-U without obtaining them from a separation step
    # However, only a restrictive set of parametric copulas is used in practice
    copula = CMAseparation( X , p_ ) # result contains xdd, udd, and the JxN copula U. U is the copula with dimension J x N
    
# merge panic copula with normal marginals (using random number generation theory / Monte Carlo)
    library(matlab)
    y = NULL # initialize variables for subsequent r-binding
    u = NULL # initialize variables for subsequent r-binding
    for ( n in 1:N )
    {    
        # generate a sequence 
        # create a 100 x 1 linearly spaced vector (i.e. 'even' sample from uniform distribution +/- 4 s.d.'s )
        yn = as.matrix( linspace( -4 * sig , 4 * sig , 100 ) )      
        # we calculate the corresponding normal cdf values 
        un = pnorm( yn , 0 , sig ) # measure P( yn <= x) using the cumulative density function using the uniform sample from yn. The plot is a cdf with range (epsilon : 1-epsilon). 100 x 1 vector.    
        
        if ( is.null( y ) ) 
        { 
            y = yn 
            u = un      
        } 
        else 
        {
            y = cbind( y , yn ) # y is 100 x N. each column of y is identical and in ascending order. the start value is -4*sig, the tail value is 4*sig
            u = cbind( u , un ) # u is 100 x N. each column of u i identical and in ascending order. The start value is e and tail value is 1 - e
        }
    }
    
# merge a new arbitrary marginal defined by 'y' and its cdf 'u'. 
    # we created linearly spaced vectors for the purposes of linear interpolation
    # The copula of Y will have the same copula as the copula of X (which is result$U)
    Y = CMAcombination( y , u , copula$U ) # Y is a J x N matrix where the minimum in each column is -.8, and the maximum is .8
    
#  compute portfolio risk
    w = matlab:::ones( N , 1 ) / N  # N x 1 matrix of portfolio weights    
    # J x 1 matrix consisting of the portfolio return distribution based on
    # the new joint distribution (assumng equal weights)
    R_w = Y %*% w # Range is from -.18 to .18
    meanReturn = mean( R_w )
    
    histOld = ProbabilityWeightedHistogram( R_w , p , round( 10 * log(J)) , freq = FALSE ) # barplot with old probabilities
    histOld = ProbabilityWeightedHistogram( R_w , p_ , round( 10 * log(J)) , freq = FALSE ) # barplot with old probabilities
    browser()
    print( histOld$Plot )
    print( histNew$Plot )
    # abline( v = mean( R_w ) , col = "blue" )
    # n = hist$np # returns new probabilities "np" -- length round( 10 * log(J))
    # D = hist$x # returns breaks in histogram -- length round( 10 * log(J))
    
# correlation of mean return and variance    
    varianceReturns = var( R_w )
    print( c("Mean Return" , meanReturn ) )
    print( c("Variance of Returns" , varianceReturns ) )
    
#barHist = barplot ( height = n[order(D)] , width = diff( D , differences = 1) , axes = TRUE , axisnames = TRUE )
    # barHist = barplot ( n[order(D)] , D , axes = TRUE , axisnames = TRUE )
    # barHist = barplot ( n , D ) # plot probabilities on Y-Axis. Note - MATLAB and R switch order of first two arguments. h = bar( D , n , 1 ). Todo: What does third argument in bar do?
    # [x_lim]=get(gca,'xlim')
    # set(gca,'ytick',[])
    # set(h,'FaceColor',.5*[1 1 1],'EdgeColor',.5*[1 1 1]);
    # grid on
    
# rotate panic copula (only 2Dim)
    if ( N == 2 )
    { 
        th = pi / 2 
        R = matrix( c( cos(th) , sin(th) , -sin(th) , cos(th) ) , byrow = TRUE , ncol = 2 )   # create rotation matrix
        X_ = X %*% t( R )
        twodimAssetResult = CMAseparation( X_ , p_ ) # returns [xdd,udd,U_]
    }
    
    return ( list( copula = copula , p_ = invisible( p_ ) , meanReturn = meanReturn , varianceReturns = varianceReturns ) ) 
}

# base case
#result = PanicCopula( N = 20 , J = 50000 , r_c = .3 , r = .99 , b = .02 , sig = .2 , sigma )

# persp(result$copula$udd[,1],result$copula$udd[,2],result$copula[,1:2], theta=30, phi=30, expand=0.6, col='lightblue', shade=0.75, ltheta=120, ticktype='detailed')

# 0% probability of panic
#PanicCopula( N = 20 , J = 50000 , r_c = .3 , r = .99 , b = 0 , sig = .2 , sigma )

# 15% probability of panic
# result = PanicCopula( N = 20 , J = 50000 , r_c = .3 , r = .85 , b = .15 , sig = .2 , sigma )

# 33% probability of volatile market
#PanicCopula( N = 20 , J = 50000 , r_c = .3 , r = .5 , b = .5 , sig = .2 , sigma )

# panic scenario = calm market scenario
#result = PanicCopula( N = 20 , J = 50000 , r_c = .3 , r = .3 , b = 0 , sig = .2 , sigma )

# Todos - impose "hard" views by restricting the variance of the mean forecasts
# restrict the variability of the forecast to match the historical sample variance of the 
# model forecasts, thereby imposing the same precision as the model

# wireframe( result$copula$U[1:10,1:20] )
# wireframe( result$copula$U[1:1,1:2] )
# levelplot( result$copula$xdd[1:10,1:20] )
# contourplot( result$copula$xdd[1:100,1:20] )
# show exploratory analysis plots. TODO
# hist( p_ , 100 ) # notice the prior probability 'p' is uniformly distributed, whereas p_ is normal with a left tail    
# hist( result$copula$udd[,1] , 100 , ylim = c( 0 , 600 ) ) # notice 'u' is not a uniform distribution    
# plot( y[ , 1 ] , type = "l" ) # straight line from -4*sig to 4*sig
# plot( u[ , 1 ] , type = "l" ) # a normal cdf. x-axis 1:100, y-axis 0:1. Interpretation: CDF of generic 'y' variable. NOT the grade since 'u' is normally distributed
# plot( y[ , 1] , u[ , 1 ] , type = "l" ) # Interpretation: Normal CDF. y-axis is the cumulative probability of the generic x    
# plot Copula
# scatterplot3d( copula$U )
