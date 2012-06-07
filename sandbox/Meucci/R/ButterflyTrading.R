# TODO: plot efficient frontier in R

PlotFrontier = function( e , s , w )
{
    # subplot(2,1,1)
    plot( s , e )
    # grid on
    # set(gca,'xlim',[min(s) max(s)])
    # 
    # subplot(2,1,2)
    
    xx = nrow( w ) ; N = ncol( w )
    Data = apply( w , 1 , cumsum ) #TODO: Check. Take cumulative sum of *rows*. Try sapply?  
    
    for ( n in 1:N ) 
    {
        x = cbind( min(s) , s , max(s) )
        y = cbind( 0 , Data[ , N-n+1 ] , 0 )
        # hold on
        #h = fill( x , y , cbind( .9 , .9 , .9) - mod( n , 3 ) %*% cbind( .2 , .2 , .2) )
    }
    
    #set(gca,'xlim',[min(s) max(s)],'ylim',[0 max(max(Data))])
    #xlabel('portfolio # (risk propensity)')
    #ylabel('portfolio composition')
}

ViewCurveSlope = function( X , p )
{
    # view 3 (expectations and binding constraints): slope of the yield curve will increase by 5 bp
    
    J = nrow( X ) ; K = ncol( X )
    
    # constrain probabilities to sum to one...
    Aeq = ones( 1 , J )
    beq = 1
    
    # ...constrain the expectation...
    V = X[ , 14 ] - X[ , 13 ]
    v = .0005
    
    Aeq = rbind( Aeq , t(V) )
    
    beq = rbind( beq , v )
    
    A = b = emptyMatrix  
    
    # ...compute posterior probabilities
    p_ = EntropyProg( p , A , b , Aeq ,beq )$p_  
    return( p_ )
}

ViewRealizedVol = function( X , p )
{  
    # view 2 (relative inequality view on median): bullish on realized volatility of MSFT (i.e. absolute log-change in the underlying). 
    # This is the variable such that, if larger than a threshold, a long position in the butterfly turns into a profit (e.g. Rachev 2003)
    # we issue a relative statement on the media comparing it with the third quintile implied by the reference market model
    
    library( matlab )
    J = nrow( X ) ; K = ncol( X )
    
    # constrain probabilities to sum to one...
    Aeq = ones( 1 , J )
    beq = 1
    
    # ...constrain the median...  
    V = abs( X[ , 1 ] ) # absolute value of the log of changes in MSFT close prices (definition of realized volatility)  
    
    V_Sort = sort( V , decreasing = FALSE ) # sorting of the abs value of log changes in prices from smallest to largest
    I_Sort = order( V ) 
    
    F = cumsum( p[ I_Sort ] ) # represents the cumulative sum of probabilities from ~0 to 1
    
    I_Reference = max( matlab:::find( F <= 3/5 ) ) # finds the (max) index corresponding to element with value <= 3/5 along the empirical cumulative density function for the abs log-changes in price
    V_Reference = V_Sort[ I_Reference ] # returns the corresponding abs log of change in price at the 3/5 of the cumulative density function
    
    I_Select = find( V <= V_Reference ) # finds all indices with value of abs log-change in price less than the reference value
    
    a = zeros( 1 , J )
    a[ I_Select ] = 1 # select those cases where the abs log-change in price is less than the 3/5 of the empirical cumulative density...
    
    A = a
    b = .5 # ... and assign the probability of these cases occuring as 50%. This moves the media of the distribution
    
    # ...compute posterior probabilities
    p_ = EntropyProg( p , A , b , Aeq , beq )$p_
    
    return( p_ )
}

ViewImpliedVol = function( X , p )
{    
    # View 1 (inequality view): bearish on on 2m-6m implied volaility spread for Google
    
    J = nrow( X ) ; K = ncol( X )
    
    # constrain probabilities to sum to one...
    Aeq = ones( 1 , J )
    beq = 1 
    
    # ...constrain the expectation...
    V = X[ , 12 ] - X[ , 11 ] # GOOG_vol_182 (6m implied vol) - GOOG_vol_91 (2m implied vol)
    m = mean( V )
    s = std( V )
    
    A = t( V )
    b = m - s
    
    # ...compute posterior probabilities
    p_ = EntropyProg( p , A , b , Aeq , beq )$p_
    
    return( p_ )
}

ComputeCVaR = function( Units , Scenarios , Conf )
{
    PnL = Scenarios %*% Units
    Sort_PnL = PnL[ order( PnL , decreasing = FALSE ) ] # DOUBLE CHECK IF I SHOULD USE ORDER INSTEAD OF SORT
    
    J = length( PnL )
    Cut = round( J %*% ( 1 - Conf ) , 0 )
    
    CVaR = -mean( Sort_PnL[ 1:Cut ] )
    
    return( CVaR )
}

LongShortMeanCVaRFrontier = function( PnL , Probs , Butterflies , Options )
{
    library( matlab )
    library( quadprog )
    library( limSolve )
    
    # setup constraints
    J = nrow(PnL); N = ncol(PnL)
    P_0s = matrix(  , nrow = 1 , ncol = 0 )
    D_s  = matrix(  , nrow = 1 , ncol = 0 )
    emptyMatrix = matrix( nrow = 0 , ncol = 0 )
    
    for ( n in 1:N )
    {
        P_0s = cbind( P_0s , Butterflies[[n]]$P_0 ) # 1x9 matrix
        D_s = cbind( D_s , Butterflies[[n]]$Delta ) # 1x9 matrix
    }
    
    Constr = list()
    Constr$Aeq = P_0s # linear coefficients in the constraints Aeq*X = beq (equality constraints)
    Constr$beq = Options$Budget # the constant vector in the constraints Aeq*x = beq
    
    if ( Options$DeltaNeutral == TRUE ) 
    {
        Constr$Aeq = rbind( Constr$Aeq , D_s ) # 2x9 matrix
        Constr$beq = rbind( Constr$beq , 0 )   # 2x9 matrix
    }
    
    Constr$Aleq = rbind( diag( as.vector( P_0s ) ) , -diag( as.vector( P_0s ) ) ) # linear coefficients in the constraints A*x <= b. an 18x9 matrix
    Constr$bleq = rbind( Options$Limit * ones(N,1) , Options$Limit * ones(N,1) ) # constant vector in the constraints A*x <= b. an 18x1 matrix
    
    # determine expectation of minimum-variance portfolio
    Exps = t(PnL) %*% Probs
    Scnd_Mom = t(PnL) %*% (PnL * (Probs %*% ones(1,N) ) )
    Scnd_Mom = ( Scnd_Mom + t(Scnd_Mom) ) / 2
    Covs = Scnd_Mom - Exps %*% t(Exps)
    
    Amat = rbind( Constr$Aeq , Constr$Aleq ) # stack the equality constraints on top of the inequality constraints
    bvec = rbind( Constr$beq , Constr$bleq ) # stack the equality constraints on top of the inequality constraints
    
    #if ( nrow(Covs) != length( zeros(N,1) ) ) stop("Dmat and dvec are incompatible!")
    #if ( nrow(Covs) != nrow(Amat)) stop("Amat and dvec are incompatible!")
    
    MinSDev_Units = solve.QP( Dmat = Covs , dvec = -1 * zeros(N,1) , Amat = -1*t(Amat) , bvec = -1*bvec , meq = length( Constr$beq) ) # TODO: Check this
    MinSDev_Exp = t( MinSDev_Units$solution ) %*% Exps
    
    # determine expectation of maximum-expectation portfolio
    
    MaxExp_Units = linp( E = Constr$Aeq , F = Constr$beq , G = -1*Constr$Aleq , H = -1*Constr$bleq , Cost = -Exps , ispos = FALSE )$X 
    
    MaxExp_Exp = t( MaxExp_Units ) %*% Exps
    
    # slice efficient frontier in NumPortf equally thick horizontal sections
    Grid = t( seq( from = Options$FrontierSpan[1] , to = Options$FrontierSpan[2] , length.out = Options$NumPortf ) )
    TargetExp = as.numeric( MinSDev_Exp ) + Grid * as.numeric( ( MaxExp_Exp - MinSDev_Exp ) )
    
    # compute composition, expectation, s.dev. and CVaR of the efficient frontier
    Composition = matrix( , ncol = N , nrow = 0 )
    Exp = matrix( , ncol = 1 , nrow = 0 )
    SDev = matrix( , ncol = 1 , nrow = 0 )
    CVaR = matrix( , ncol = 1 , nrow = 0 )
    
    for (i in 1:Options$NumPortf )
    {
        # determine least risky portfolio for given expectation
        AEq = rbind( Constr$Aeq , t(Exps) )        # equality constraint: set expected return for each asset...
        bEq = rbind( Constr$beq , TargetExp[i] )
        
        Amat = rbind( AEq , Constr$Aleq ) # stack the equality constraints on top of the inequality constraints
        bvec = rbind( bEq , Constr$bleq ) # ...and target portfolio return for i'th efficient portfolio
        
        # Why is FirstDegree "expected returns" set to 0? 
        # Becasuse we capture the equality view in the equality constraints matrix
        # In other words, we have a constraint that the Expected Returns by Asset %*% Weights = Target Return
        Units = solve.QP( Dmat = Covs , dvec = -1*zeros(N,1) , Amat = -1*t(Amat) , bvec = -1*bvec , meq = length( bEq ) )
        
        # store results
        Composition = rbind( Composition , t( Units$solution ) )
        
        Exp = rbind( Exp , t( Units$solution ) %*% Exps )
        SDev = rbind( SDev , sqrt( t( Units$solution ) %*% Covs %*% Units$solution ) )
        CVaR = rbind( CVaR , ComputeCVaR( Units$solution , PnL , Options$Quant ) )
    }   
    
    colnames( Composition ) = c( "MSFT_vol_30" , "MSFT_vol_91" , "MSFT_vol_182" , 
            "YHOO_vol_30" , "YHOO_vol_91" , "YHOO_vol_182" ,    
            "GOOG_vol_30" , "GOOG_vol_91" , "GOOG_vol_182" )
    
    return( list( Exp = Exp , SDev = SDev , CVaR = CVaR , Composition = Composition ) )
}


MapVol = function( sig , y , K , T )
{
    # in real life a and b below should be calibrated to security-specific time series
    
    a=-.00000000001
    b= .00000000001 
    
    s = sig + a/sqrt(T) * ( log(K) - log(y) ) + b/T*( log(K) - log(y) )^2
    
    return( s )
}

HorizonPricing = function( Butterflies , X )
{
    r = .04       # risk-free rate
    tau = 1/252   # investment horizon
    
#  factors: 1. 'MSFT_close'   2. 'MSFT_vol_30'   3. 'MSFT_vol_91'   4. 'MSFT_vol_182'
#  securities:                1. 'MSFT_vol_30'   2. 'MSFT_vol_91'   3. 'MSFT_vol_182'
    
# create a new row called DlnY and Dsig
# create a new row called 'DlnY'. Assign the first row (vector) of X to this DlnY for the 1:3 securities
    for ( s in 1:3 ) { Butterflies[[s]]$DlnY = X[ , 1 ] }
    
# assign the 2nd row of X to a new element called Dsig
    Butterflies[[1]]$Dsig=X[ , 2 ]
    Butterflies[[2]]$Dsig=X[ , 3 ]
    Butterflies[[3]]$Dsig=X[ , 4 ]
    
#  factors:  5. 'YHOO_close'   6. 'YHOO_vol_30'   7. 'YHOO_vol_91'   8. 'YHOO_vol_182'
#  securities:                 4. 'YHOO_vol_30'   5. 'YHOO_vol_91'   6. 'YHOO_vol_182'
    for ( s in 4:6 ) { Butterflies[[s]]$DlnY=X[ , 5 ] }
    
    Butterflies[[4]]$Dsig=X[ , 6 ]
    Butterflies[[5]]$Dsig=X[ , 7 ]
    Butterflies[[6]]$Dsig=X[ , 8 ]
    
#  factors:  #  9. 'GOOG_close'  10. 'GOOG_vol_30'  11. 'GOOG_vol_91'  12. 'GOOG_vol_182'
#  securities:                    7. 'GOOG_vol_30'   8. 'GOOG_vol_91'   9.  'GOOG_vol_182'
    for ( s in 7:9 ) { Butterflies[[s]]$DlnY=X[ , 9 ] }
    
    Butterflies[[7]]$Dsig=X[ , 10 ]
    Butterflies[[8]]$Dsig=X[ , 11 ]
    Butterflies[[9]]$Dsig=X[ , 12 ]
    
    PnL = matrix( NA , nrow = nrow(X) )
    
    for ( s in 1:length(Butterflies) )
    {  
        Y = Butterflies[[s]]$Y_0 * exp(Butterflies[[s]]$DlnY)
        ATMsig = apply( cbind( Butterflies[[s]]$sig_0 + Butterflies[[s]]$Dsig , 10^-6 ) , 1 , max )     
        t = Butterflies[[s]]$T - tau
        K = Butterflies[[s]]$K
        sig = MapVol(ATMsig , Y , K , t )
        
        # library(RQuantLib) # this function can only operate on one option at a time, so we use fOptions    
        # C = EuropeanOption( type = "call" , underlying = Y , strike = K , dividendYield = 0 , riskFreeRate = r , maturity = t , volatility = sig )$value
        # P = EuropeanOption( type = "put" ,  underlying = Y , strike = K , dividendYield = 0 , riskFreeRate = r , maturity = t , volatility = sig )$value
        
        # use fOptions to value options
        library( fOptions )
        C = GBSOption( TypeFlag = "c" , S = Y , X = K , r = r , b = 0 , Time = t , sigma = sig  )
        P = GBSOption( TypeFlag = "p" , S = Y , X = K , r = r , b = 0 , Time = t , sigma = sig  )    
        
        Butterflies[[s]]$P_T = C@price + P@price
        PnL = cbind( PnL , Butterflies[[s]]$P_T )
    }
    PnL = PnL[ , -1 ]
}

