#' Entropy pooling program for blending views on scenarios with a prior scenario-probability distribution
#'
#' Entropy program will change the initial predictive distribution 'p' to a new set 'p_' that satisfies
#' specified moment conditions but changes other propoerties of the new distribution the least by
#' minimizing the relative entropy between the two distributions. Theoretical note: Relative Entropy (Kullback-Leibler information criterion KLIC) is an
#' asymmetric measure. 
#'
#' We retrieve a new set of probabilities for the joint-scenarios using the Entropy pooling method
#' Of the many choices of 'p' that satisfy the views, we choose 'p' that minimize the entropy or distance of the new probability
#' distribution to the prior joint-scenario probabilities
#' We use Kullback-Leibler divergence or relative entropy dist(p,q): Sum across all scenarios [ p-t * ln( p-t / q-t ) ]
#' Therefore we define solution as p* = argmin (choice of p ) [ sum across all scenarios: p-t * ln( p-t / q-t) ], 
#' such that 'p' satisfies views. The views modify the prior in a cohrent manner (minimizing distortion)
#' We forumulate the stress tests of the baseline scenarios as linear constraints on yet-to-be defined probabilities
#' Note that the numerical optimization acts on a very limited number of variables equal
#' to the number of views. It does not act directly on the very large number of variables
#' of interest, namely the probabilities of the Monte Carlo scenarios. This feature guarantees
#' the numerical feasability of entropy optimization
#' Note that new probabilities are generated in much the same way that the state-price density modifies
#' objective probabilities of pay-offs to risk-neutral probabilities in contingent-claims asset pricing
#'
#' Compute posterior (=change of measure) with Entropy Pooling, as described in
#'
#' @param  p        a vector of initial probabilities based on prior (reference model, empirical distribution, etc.). Sum of 'p' must be 1
#' @param  Aeq      matrix consisting of equality constraints (paired with argument 'beq'). Denoted as 'H' in the Meucci paper. (denoted as 'H' in the "Meucci - Flexible Views Theory & Practice" paper formlua 86 on page 22)
#' @param  beq      vector corresponding to the matrix of equality constraints (paired with argument 'Aeq'). Denoted as 'h' in the Meucci paper
#' @param  A        matrix consisting of inequality constraints (paired with argument 'b'). Denoted as 'F' in the Meucci paper
#' @param  b        vector consisting of inequality constraints (paired with matrix A). Denoted as 'f' in the Meucci paper
#'
#' ' \deqn{ \tilde{p}  \equiv  argmin_{Fx \leq f, Hx  \equiv  h}  \big\{ \sum_1^J  x_{j}  \big(ln \big( x_{j} \big) - ln \big( p_{j} \big) \big)  \big\} 
#' \\ \ell  \big(x,  \lambda,  \nu \big)  \equiv  x'  \big(ln \big(x\big) - ln \big(p\big) \big) +   \lambda' \big(Fx - f\big)  +   \nu' \big(Hx - h\big)}
#' @return a list with 
#'      p_                       revised probabilities based on entropy pooling
#'      optimizationPerformance  a list with status of optimization, value, number of iterations and sum of probabilities.
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @references 
#' A. Meucci - "Fully Flexible Views: Theory and Practice". See page 22 for illustration of numerical implementation
#' Symmys site containing original MATLAB source code \url{http://www.symmys.com}
#' NLOPT open-source optimization site containing background on algorithms \url{http://ab-initio.mit.edu/wiki/index.php/NLopt}
#' We use the information-theoretic estimator of Kitamur and Stutzer (1997). 
#' Reversing 'p' and 'p_' leads to the empirical likelihood" estimator of Qin and Lawless (1994). 
#' See Robertson et al, "Forecasting Using Relative Entropy" (2002) for more theory
#'  @export
EntropyProg = function( p , A , b , Aeq , beq )
{
    library( nloptr )    
    
    # count the number of constraints
    K_ = nrow( A )  # K_ is the number of inequality constraints in the matrix-vector pair A-b
    K = nrow( Aeq ) # K is the number of equality views in the matrix-vector pair Aeq-beq
    
    # parameter checks        
    if ( K_ + K == 0 ) { stop( "at least one equality or inequality constraint must be specified")}    
    if ( ( ( .999999 < sum(p)) & (sum(p) < 1.00001) ) == FALSE ) { stop( "sum of probabilities from prior distribution must equal 1")}            
    if ( nrow(Aeq)!=nrow(beq) ) { stop( "number of inequality constraints in matrix Aeq must match number of elements in vector beq") }
    if ( nrow(A)!=nrow(b) ) { stop( "number of equality constraints in matrix A must match number of elements in vector b") }              
    
    # calculate derivatives of constraint matrices
    A_ = t( A )
    b_= t( b )    
    Aeq_ = t( Aeq )
    beq_ = t( beq )        
    
    # starting guess for optimization search with length = to number of views
    x0 = matrix( 0 , nrow = K_ + K , ncol = 1 ) 
    
    if ( !K_ ) # equality constraints only    
    {    
        # define maximum likelihood, gradient, and hessian functions for unconstrained and constrained optimization    
        eval_f_list = function( v ) # cost function for unconstrained optimization (no inequality constraints)
        {
            x = exp( log(p) - 1 - Aeq_ %*% v )
            x = apply( cbind( x , 10^-32 ) , 1 , max )  # robustification
            # L is the Lagrangian dual function (without inequality constraints). See formula 88 on p. 22 in "Meucci - Fully Flexible Views - Theory and Practice (2010)"
            # t(x) is the derivative x'
            # Aeq_ is the derivative of the Aeq matrix of equality constraints (denoted as 'H in the paper)
            # beq_ is the transpose of the vector associated with Aeq equality constraints  
            # L=  x'  *  ( log(x) - log(p) + Aeq_  *  v ) -   beq_ *  v
            #    1xJ  *   ( Jx1   - Jx1  + JxN+1 * N+1x1 ) - 1xN+1 * N+1x1    
            L = t(x) %*% ( log(x) - log(p) + Aeq_ %*% v ) - beq_ %*% v
            mL = -L # take negative values since we want to maximize
            
            # evaluate gradient
            gradient = beq - Aeq %*% x
            
            # evaluate Hessian
            # We comment this out for now -- to be used when we find an optimizer that can utilize the Hessian in addition to the gradient
            # H = ( Aeq %*% (( x %*% ones(1,K) ) * Aeq_) ) # Hessian computed by Chen Qing, Lin Daimin, Meng Yanyan, Wang Weijun
            
            return( list( objective = mL , gradient = gradient ) )
        }         
        
        # setup unconstrained optimization
        start = Sys.time()    
        opts = list( algorithm = "NLOPT_LD_LBFGS" , xtol_rel = 1.0e-6 , 
                check_derivatives = TRUE , check_derivatives_print = "all" , print_level = 2 , maxeval = 1000 )    
        optimResult = nloptr(x0 = x0, eval_f = eval_f_list , opts = opts )    
        end = Sys.time()
        print( c("Optimization completed in " , end - start )) ; rm( start ) ; rm( end )
        
        if ( optimResult$status < 0 ) { print( c("Exit code " , optimResult$status ) ) ; stop( "Error: The optimizer did not converge" ) }
        
        # return results of optimization
        v = optimResult$solution
        p_ = exp( log(p) - 1 - Aeq_ %*% v ) 	    
        optimizationPerformance = list( converged = (optimResult$status > 0) , ml = optimResult$objective , iterations = optimResult$iterations , sumOfProbabilities = sum( p_ ) )        
    }
    
    else # case inequality constraints are specified    
    {        
        # setup variables for constrained optimization
        InqMat = -diag( 1 , K_ + K ) # -1 * Identity Matrix with dimension equal to number of constraints
        InqMat = InqMat[ -c( K_+1:nrow( InqMat ) ) , ] # drop rows corresponding to equality constraints
        InqVec = matrix( 0 , K_ , 1 )
        
        # define maximum likelihood, gradient, and hessian functions for constrained optimization    
        InqConstraint = function( x ) { return( InqMat %*% x ) } # function used to evalute A %*% x <= 0 or A %*% x <= c(0,0) if there is more than one inequality constraint
        
        jacobian_constraint = function( x ) { return( InqMat ) } 
        # Jacobian of the constraints matrix. One row per constraint, one column per control parameter (x1,x2)
        # Turns out the Jacobian of the constraints matrix is always equal to InqMat
        
        nestedfunC = function( lv )
        {           
            lv = as.matrix( lv )    
            l = lv[ 1:K_ , , drop = FALSE ] # inequality Lagrange multiplier
            v = lv[ (K_+1):length(lv) , , drop = FALSE ] # equality lagrange multiplier
            x = exp( log(p) - 1 - A_ %*% l - Aeq_ %*% v )
            x = apply( cbind( x , 10^-32 ) , 1 , max )  
            
            # L is the cost function used for constrained optimization
            # L is the Lagrangian dual function with inequality constraints and equality constraints
            L = t(x) %*% ( log(x) - log(p) ) + t(l) %*% (A %*% x-b) + t(v) %*% (Aeq %*% x-beq)    
            objective = -L  # take negative values since we want to maximize
            
            # calculate the gradient
            gradient = rbind( b - A%*%x , beq - Aeq %*% x )       
            
            # compute the Hessian (commented out since no R optimizer supports use of Hessian)
            # Hessian computed by Chen Qing, Lin Daimin, Meng Yanyan, Wang Weijun    
            #onesToK_ = array( rep( 1 , K_ ) ) ;onesToK = array( rep( 1 , K ) )            
            #x = as.matrix( x )            
            #H11 = A %*% ((x %*% onesToK_) * A_) ;  H12 = A %*% ((x %*% onesToK) * Aeq_)
            #H21 = Aeq %*% ((x %*% onesToK_) * A_) ; H22 = Aeq %*% ((x %*% onesToK) * Aeq_)
            #H1 = cbind( H11 , H12 ) ; H2 = cbind( H21 , H22 ) ; H = rbind( H1 , H2 ) # Hessian for constrained optimization            
            return( list( objective = objective , gradient = gradient ) )  
        }
        
        # find minimum of constrained multivariate function        
        start = Sys.time()
        # Note: other candidates for constrained optimization in library nloptr: NLOPT_LD_SLSQP, NLOPT_LD_MMA, NLOPT_LN_AUGLAG, NLOPT_LD_AUGLAG_EQ
        # See NLOPT open-source site for more details: http://ab-initio.mit.edu/wiki/index.php/NLopt
        local_opts <- list( algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1.0e-6 , 
                check_derivatives = TRUE , check_derivatives_print = "all" , 
                eval_f = nestedfunC , eval_g_ineq = InqConstraint , eval_jac_g_ineq = jacobian_constraint )
        optimResult = nloptr( x0 = x0 , eval_f = nestedfunC , eval_g_ineq = InqConstraint , eval_jac_g_ineq = jacobian_constraint ,
                opts = list( algorithm = "NLOPT_LD_AUGLAG" , local_opts = local_opts ,
                        print_level = 2 , maxeval = 1000 , 
                        check_derivatives = TRUE , check_derivatives_print = "all" , xtol_rel = 1.0e-6 ) )
        end = Sys.time()
        print( c("Optimization completed in " , end - start )) ; rm( start ) ; rm( end )    
        
        if ( optimResult$status < 0 ) { print( c("Exit code " , optimResult$status ) ) ; stop( "Error: The optimizer did not converge" ) }       
        
        # return results of optimization
        lv = matrix( optimResult$solution , ncol = 1 )
        l = lv[ 1:K_ , , drop = FALSE ] # inequality Lagrange multipliers
        v = lv[ (K_+1):nrow( lv ) , , drop = FALSE ] # equality Lagrange multipliers
        p_ = exp( log(p) -1 - A_ %*% l - Aeq_ %*% v )            
        optimizationPerformance = list( converged = (optimResult$status > 0) , ml = optimResult$objective , iterations = optimResult$iterations , sumOfProbabilities = sum( p_ ) )
    }
    
    print( optimizationPerformance )
    
    if ( sum( p_ ) < .999 ) { stop( "Sum or revised probabilities is less than 1!" ) }
    if ( sum( p_ ) > 1.001 ) { stop( "Sum or revised probabilities is greater than 1!" ) }
    
    return ( list ( p_ = p_ , optimizationPerformance = optimizationPerformance ) )
}

#' Calculate the full-confidence posterior distributions of Mu and Sigma
#'
#' \deqn{ \tilde{ \mu }  \equiv \mu +  \Sigma  Q'    {\big(Q \Sigma  Q' \big)}^{-1}   \big( \tilde{\mu}_{Q} - Q \mu \big),
#' \\ \tilde{ \Sigma } \equiv \Sigma + \Sigma G' \big({\big(G \Sigma  G' \big)}^{-1} \tilde{ \Sigma }_G {\big(G \Sigma  G' \big)}^{-1} - {\big(G \Sigma  G' \big)}^{-1} \big) G \Sigma }
#' @param M     a numeric vector with the Mu of the normal reference model
#' @param Q     a numeric vector used to construct a view on expectation of the linear combination QX
#' @param M_Q   a numeric vector with the view of the expectations of QX
#' @param S     a covariance matrix for the normal reference model
#' @param G     a numeric vector used to construct a view on covariance of the linear combination GX
#' @param S_G   a numeric with the expectation associated with the covariance of the linear combination GX
#'
#' @return a list with 
#' @return M_   a numeric vector with the full-confidence posterior distribution of Mu
#' @return S_   a covariance matrix with the full-confidence posterior distribution of Sigma
#'
#' @references 
#' \url{http://www.symmys.com}
#' \url{http://ssrn.com/abstract=1213325}
#' A. Meucci - "Fully Flexible Views: Theory and Practice". See formula (21) and (22) on page 7
#' See Meucci script Prior2Posterior.m attached to Entropy Pooling Paper
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
Prior2Posterior = function( M , Q , M_Q , S , G , S_G )
{
  # Compute posterior moments
  
  if ( Q != 0 ) { M_ = M + S %*% t(Q) %*% solve( Q %*% S %*% t(Q) ) %*% ( M_Q - Q %*% M) }
  else { M_ = M }
  
  if ( G != 0 ) { S_ = S + (S %*% t(G)) %*% ( solve(G %*% S %*% t(G)) %*% S_G %*% solve(G %*% S %*% t(G)) - solve( G %*% S %*% t(G)) ) %*% (G %*% S) }
  else { S_ = S }
  
  return( list( M_ = M_ , S_ = S_ ) )
}

#' Generates histogram
#'
#' @param X       a vector containing the data points
#' @param p       a vector containing the probabilities for each of the data points in X
#' @param nBins   expected number of Bins the data set is to be broken down into
#' @param freq    a boolean variable to indicate whether the graphic is a representation of frequencies
#'
#' @return a list with 
#'             f   the frequency for each midpoint
#'             x   the midpoints of the nBins intervals
#'
#' @references 
#' \url{http://www.symmys.com}
#' See Meucci script pHist.m used for plotting
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}

pHist = function( X , p , nBins, freq = FALSE )    
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
  
  return( list( f = f , x = x ) )
}
